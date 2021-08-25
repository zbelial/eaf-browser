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
(require 'db)

(defcustom eaf-browser-enable-annotator nil
  ""
  :type 'boolean)
;; (setq eaf-browser-enable-annotator t)

(defcustom eaf-browser-enable-annotator-tags-plugin t
  ""
  :type 'boolean)

(defcustom eaf-browser-enable-annotator-markdown-plugin nil
  ""
  :type 'boolean)

(defcustom eaf-browser-annotator-db-location (expand-file-name (locate-user-emacs-file "eaf/db/"))
  "A directory to store db files to."
  :type 'directory)

(defvar eba--dbs (make-hash-table :test #'eq))

(defun eba--get-db (id)
  (let ((db (gethash id eba--dbs)))
    (unless db
      (setq db (db-make `(db-hash
                          :file-name ,(format "%s%s" eaf-browser-annotator-db-location id))))
      (puthash id db eba--dbs)
      )
    db
    )
  )

(defun eba--new-id ()
  (format "%d" (random 99999999))
  )

(defun eba--save (annotation)
  )

;; "{'highlights': [{'jQuery19109976706928828596': 30}], 'quote': '板块快速冲高，情绪非常高亢，很', 'ranges': [{'end': '/div[1]/div[1]/div[1]/div[2]/div[1]/div[1]/div[1]/div[1]/div[2]/p[3]', 'endOffset': 34, 'start': '/div[1]/div[1]/div[1]/div[2]/div[1]/div[1]/div[1]/div[1]/div[2]/p[3]', 'startOffset': 19}], 'tags': [], 'text': ''}"
(defun eaf-browser-annotator-create (db-id annotation)
  (message "create annotation %S" annotation)
  (let ((decoded (json-parse-string annotation))
        (anno-id (eba--new-id))
        (db (eba--get-db db-id))
        encoded)
    (message "decoded %S" decoded)
    (puthash "id" anno-id decoded)

    (setq encoded (json-encode-hash-table decoded))

    (message "encoded %s" encoded)

    (db-put anno-id encoded db)

    encoded
    )
  )

(defun eaf-browser-annotator-update (db-id anno-id annotation)
  (message "update annotation db-id %s, anno-id %s, annotation %S" db-id anno-id annotation)
  )

(defun eaf-browser-annotator-delete (db-id anno-id)
  (message "delete annotation db-id %s, anno-id %s" db-id anno-id)
  )

(defun eaf-browser-annotator-load (file-full-name file-name-md5)
  (message "load annotations %s, %s" file-full-name file-name-md5)
  )

(provide 'eaf-browser-annotator)
