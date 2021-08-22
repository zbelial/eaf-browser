;;; eaf-browser-annotator.el --- Annotation support in Browser plugins -*- lexical-binding: t; -*-

;; Filename: eaf-browser-annotator.el
;; Description: Browser plugins
;; Author: zbelial <zjyzhaojiyang@gmail.com>
;; Maintainer: zbelial <zjyzhaojiyang@gmail.com>
;; Copyright (C) 2021, zbelial, all rights reserved.
;; Created: 2021-07-20 22:30:28
;; Version: 0.1
;; Last-Updated: Sat Aug 21 11:30:12 2021 (+0800)
;;           By: zbelial
;; URL: 
;; Keywords:
;; Compatibility: GNU Emacs 28.0.50
;;
;; Features that might be required by this library:
;;
;;
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Annotation support in Browser plugins
;;

(require 'cl-lib)
(require 'web-server)

(defvar eaf-browser-annotator--webserver-instance nil
  "")

(defvar eaf-browser-annotator-server-port nil
  "Port which web-server listens.")

;;;###autoload
(defun ws-start2 (handlers &optional port log-buffer &rest network-args)
  "Start a server using HANDLERS and return the server object.

HANDLERS may be a single function (which is then called on every
request) or a list of conses of the form (MATCHER . FUNCTION),
where the FUNCTION associated with the first successful MATCHER
is called.  Handler functions are called with two arguments, the
process and the request object.

A MATCHER may be either a function (in which case it is called on
the request object) or a cons cell of the form (KEYWORD . STRING)
in which case STRING is matched against the value of the header
specified by KEYWORD.

Any supplied NETWORK-ARGS are assumed to be keyword arguments for
`make-network-process' to which they are passed directly.

For example, the following starts a simple hello-world server on
port 8080.

  (ws-start2
   (lambda (request)
     (with-slots (process headers) request
       (process-send-string process
        \"HTTP/1.1 200 OK\\r\\nContent-Type: text/plain\\r\\n\\r\\nhello world\")))
   8080)

Equivalently, the following starts an identical server using a
function MATCH and the `ws-response-header' convenience
function.

  (ws-start2
   `(((lambda (_) t) .
      (lambda (request)
        (with-slots ((proc process)) request
          (ws-response-header proc 200 '(\"Content-Type\" . \"text/plain\"))
          (process-send-string proc \"hello world\")))))
   8080)

"
  (let ((server (make-instance 'ws-server :handlers handlers :port port))
        (log (when log-buffer (get-buffer-create log-buffer))))
    (setf (ws-process server)
          (apply
           #'make-network-process
           :name "ws-server"
           :service (if port port t)
           :filter 'ws-filter
           :server t
           :nowait (< emacs-major-version 26)
           :family 'ipv4
           :coding 'no-conversion
           :plist (append (list :server server)
                          (when log (list :log-buffer log)))
           :log (when log
                  (lambda (proc request message)
                    (let ((c (process-contact request))
                          (buf (plist-get (process-plist proc) :log-buffer)))
                      (with-current-buffer buf
                        (goto-char (point-max))
                        (insert (format "%s\t%s\t%s\t%s"
                                        (format-time-string ws-log-time-format)
                                        (cl-first c) (cl-second c) message))))))
           network-args))
    (when (null port)
      (setf (ws-port server) (process-contact (ws-process server) :service))
      )
    (push server ws-servers)
    server))

(defun eaf-browser-annotator--is-another-server-running()
  (when eaf-browser-annotator-server-port
    (let ((url (format "http://localhost:%s/_status" eaf-browser-annotator-server-port))
          running)
      (request url
        :type "GET"
        :encoding 'utf-8
        :sync t
        :success (cl-function
                  (lambda (&key data &allow-other-keys)
                    (setq running t)))
        :error (cl-function
                (lambda (&rest args &key error-thrown &allow-other-keys)
                  (setq running nil)))
        )
      running
      ))
  )

(defun eaf-browser-annotator--start-webserver ()
  (when (eaf-browser-annotator--is-another-server-running)
    (error (format "eaf-browser-annotator webserver is running.")))

  ;; 文件名的md5作为prefix
  (setq eaf-browser-annotator--webserver-instance
        (ws-start2
         '(((:GET . "/_status") .
            (lambda (request)
              (with-slots (process) request
                (message "web server receives status request.")
                (ws-response-header process 200 '("Content-type" . "text/plain"))
                (process-send-string process "OK\n"))))
           ;; favicon.ico
           ((:GET . "/favicon.ico$") .
            (lambda (request)
              (with-slots (process headers body) request
                (let ((docroot default-directory))
                  (message "web server receives favicon request.")
                  (message "headers %S" headers)
                  (message "body %S" body)
                  (message "docroot %s" docroot)
                  (ws-response-header process 200 '("Content-type" . "image/x-icon") '("Content-Security-Policy" . "default-src 'self' http://localhost 'unsafe-inline' 'unsafe-eval'; connect-src 'self' *; script-src 'self' http://localhost 'unsafe-inline' 'unsafe-eval';"))
                  (ws-send-file process (expand-file-name "favicon.ico" docroot))
                  )
                )))
           ;; root
           ((:GET . "/[0-9a-zA-Z]+/$") .
            (lambda (request)
              (with-slots (process) request
                (let (meta)
                  (setq meta (plist-put meta :name "EAF annotator store api"))
                  (setq meta (plist-put meta :version "1.0.0"))
                  (message "web server receives root request.")
                  (ws-response-header process 200 '("Content-type" . "application/json") '("Content-Security-Policy" . "default-src 'self' http://localhost 'unsafe-inline' 'unsafe-eval'; connect-src 'self' *; script-src 'self' http://localhost 'unsafe-inline' 'unsafe-eval';") '("Access-Control-Allow-Origin" . "*"))
                  ;; (ws-response-header process 200 '("Content-type" . "text/plain") '("Content-Security-Policy" . "default-src 'self' http://localhost 'unsafe-inline' 'unsafe-eval'; connect-src 'self' *; script-src 'self' http://localhost 'unsafe-inline' 'unsafe-eval';"))
                  (process-send-string process (json-encode meta))
                  )
                )))
           ;; index
           ((:GET . "/[0-9a-zA-Z]+/annotations$") .
            (lambda (request)
              (with-slots (process) request
                (message "web server receives index request.")
                (ws-response-header process 200 '("Content-type" . "application/json") '("Content-Security-Policy" . "default-src 'self' http://localhost 'unsafe-inline' 'unsafe-eval'; connect-src 'self' *; script-src 'self' http://localhost 'unsafe-inline' 'unsafe-eval';") '("Access-Control-Allow-Origin" . "*"))
                ;; (ws-response-header process 200 '("Content-type" . "text/plain") '("Content-Security-Policy" . "default-src 'self' http://localhost 'unsafe-inline' 'unsafe-eval'; connect-src 'self' *; script-src 'self' http://localhost 'unsafe-inline' 'unsafe-eval';"))
                (process-send-string process (json-encode-array nil))
                )))               
           ((:OPTIONS . "/[0-9a-zA-Z]+/annotations$") .
            (lambda (request)
              (with-slots (process) request
                (message "web server receives index request.")
                (ws-response-header process 200 '("Content-type" . "application/json") '("Content-Security-Policy" . "default-src 'self' http://localhost 'unsafe-inline' 'unsafe-eval'; connect-src 'self' *; script-src 'self' http://localhost 'unsafe-inline' 'unsafe-eval';") '("Access-Control-Allow-Origin" . "*"))
                ;; (ws-response-header process 200 '("Content-type" . "text/plain") '("Content-Security-Policy" . "default-src 'self' http://localhost 'unsafe-inline' 'unsafe-eval'; connect-src 'self' *; script-src 'self' http://localhost 'unsafe-inline' 'unsafe-eval';"))
                (process-send-string process (json-encode-array nil))
                )))           
           ;; create
           ((:POST . "/[0-9a-zA-Z]+/annotations$") .
            (lambda (request)
              (with-slots (process headers body) request
                (message "web server receives create request.")
                (message "headers %S" headers)
                (message "body %S" body)
                (ws-response-header process 303 '("Content-type" . "text/plain") '("Content-Security-Policy" . "default-src 'self' http://localhost 'unsafe-inline' 'unsafe-eval'; connect-src 'self' *; script-src 'self' http://localhost 'unsafe-inline' 'unsafe-eval';"))
                (process-send-string process (format "http://localhost:%d/annotations/%d" eaf-browser-annotator-server-port (random 99999999)))
                )))
           ;; read
           ((:GET . "/[0-9a-zA-Z]+/annotations/[0-9a-zA-Z]+") .
            (lambda (request)
              (with-slots (process) request
                (ws-response-header process 200 '("Content-type" . "application/json") '("Content-Security-Policy" . "default-src 'self' http://localhost 'unsafe-inline' 'unsafe-eval'; connect-src 'self' *; script-src 'self' http://localhost 'unsafe-inline' 'unsafe-eval';"))
                (process-send-string process (json-encode-array nil))
                )))
           ;; update
           ((:PUT . "/[0-9a-zA-Z]+/annotations/[0-9a-zA-Z]+") .
            (lambda (request)
              (with-slots (process) request
                (ws-response-header process 303 '("Content-type" . "application/json") '("Content-Security-Policy" . "default-src 'self' http://localhost 'unsafe-inline' 'unsafe-eval'; connect-src 'self' *; script-src 'self' http://localhost 'unsafe-inline' 'unsafe-eval';"))
                (process-send-string process (format "update %s" (random 9999)))
                )))
           ;; delete
           ((:DELETE . "/[0-9a-zA-Z]+/annotations/[0-9a-zA-Z]+") .
            (lambda (request)
              (with-slots (process) request
                (ws-response-header process 200 '("Content-type" . "application/json") '("Content-Security-Policy" . "default-src 'self' http://localhost 'unsafe-inline' 'unsafe-eval'; connect-src 'self' *; script-src 'self' http://localhost 'unsafe-inline' 'unsafe-eval';"))
                (process-send-string process (format "delete %s" (random 9999)))
                )))
           )
         8081)
        )
  (setq eaf-browser-annotator-server-port (ws-port eaf-browser-annotator--webserver-instance))
  )

(defun eaf-browser-annotator--stop-webserver ()
  (when eaf-browser-annotator--webserver-instance
    (ws-stop eaf-browser-annotator--webserver-instance)
    (setq eaf-browser-annotator--webserver-instance nil)
    (setq eaf-browser-annotator-server-port nil)
    )
  )

(defun eaf-browser-annotator-server-start ()
  (interactive)

  (eaf-browser-annotator--start-webserver)
  )

(defun eaf-browser-annotator-server-stop ()
  (interactive)
  (eaf-browser-annotator--stop-webserver)
  )

(defun eaf-browser-annotator-server-restart ()
  (interactive)
  (eaf-browser-annotator--stop-webserver)
  (eaf-browser-annotator--start-webserver)
  )

(provide 'eaf-browser-annotator)
