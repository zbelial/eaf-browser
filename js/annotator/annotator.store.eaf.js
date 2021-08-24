

//

(function() {
    var __bind = function(fn, me){ return function(){ return fn.apply(me, arguments); }; },
        __hasProp = {}.hasOwnProperty,
        __extends = function(child, parent) { for (var key in parent) { if (__hasProp.call(parent, key)) child[key] = parent[key]; } function ctor() { this.constructor = child; } ctor.prototype = parent.prototype; child.prototype = new ctor(); child.__super__ = parent.prototype; return child; },
        __indexOf = [].indexOf || function(item) { for (var i = 0, l = this.length; i < l; i++) { if (i in this && this[i] === item) return i; } return -1; };


    Annotator.Plugin.StoreEAF = (function(_super) {
        __extends(StoreEAF, _super);

        StoreEAF.prototype.events = {
            'annotationCreated': 'annotationCreated',
            'annotationDeleted': 'annotationDeleted',
            'annotationUpdated': 'annotationUpdated'
        };

        StoreEAF.prototype.options = {
            annotationData: {},
            fileMD5 : '',
        };

        function StoreEAF(element, options) {
            this._onLoadAnnotations = __bind(this._onLoadAnnotations, this);
            this._getAnnotations = __bind(this._getAnnotations, this);

            StoreEAF.__super__.constructor.apply(this, arguments);

            this.annotations = [];
        }

        StoreEAF.prototype.pluginInit = function() {
            if (!Annotator.supported()) {
                return;
            }
            if (this.annotator.plugins.Auth) {
                return this.annotator.plugins.Auth.withToken(this._getAnnotations);
            } else {
                return this._getAnnotations();
            }
        };

        StoreEAF.prototype._getAnnotations = function() {
            return this.loadAnnotations();
        };

        StoreEAF.prototype.annotationCreated = function(annotation) {
            var _this = this;
            console.log('annotationCreated');
            console.log(annotation);
            if (__indexOf.call(this.annotations, annotation) < 0) {
                this.registerAnnotation(annotation);

                eval_emacs_function('eaf-browser-annotator-create', annotation);
                // TODO
                // return this._apiRequest('create', annotation, function(data) {
                //     if (data.id == null) {
                //         console.warn(Annotator._t("Warning: No ID returned from server for annotation "), annotation);
                //     }
                //     return _this.updateAnnotation(annotation, data);
                // });
            } else {
                return this.updateAnnotation(annotation, {});
            }
        };

        StoreEAF.prototype.annotationUpdated = function(annotation) {
            var _this = this;
            console.log('annotationUpdated');
            console.log(annotation);
            if (__indexOf.call(this.annotations, annotation) >= 0) {
                // TODO
                // return this._apiRequest('update', annotation, (function(data) {
                //     return _this.updateAnnotation(annotation, data);
                // }));
            }
        };

        StoreEAF.prototype.annotationDeleted = function(annotation) {
            console.log('annotationDeleted');
            console.log(annotation);
            var _this = this;
            if (__indexOf.call(this.annotations, annotation) >= 0) {
                // TODO
                // return this._apiRequest('destroy', annotation, (function() {
                //     return _this.unregisterAnnotation(annotation);
                // }));
            }
        };

        StoreEAF.prototype.registerAnnotation = function(annotation) {
            return this.annotations.push(annotation);
        };

        StoreEAF.prototype.unregisterAnnotation = function(annotation) {
            return this.annotations.splice(this.annotations.indexOf(annotation), 1);
        };

        StoreEAF.prototype.updateAnnotation = function(annotation, data) {
            if (__indexOf.call(this.annotations, annotation) < 0) {
                console.error(Annotator._t("Trying to update unregistered annotation!"));
            } else {
                $.extend(annotation, data);
            }
            return $(annotation.highlights).data('annotation', annotation);
        };

        StoreEAF.prototype.loadAnnotations = function() {
            var fileMD5 = this.options.fileMD5
            console.log('loadAnnotations: ' + fileMD5);
            // TODO
            // return this._apiRequest('read', null, this._onLoadAnnotations);
        };

        StoreEAF.prototype._onLoadAnnotations = function(data) {
            var a, annotation, annotationMap, newData, _i, _j, _len, _len1, _ref;
            if (data == null) {
                data = [];
            }
            annotationMap = {};
            _ref = this.annotations;
            for (_i = 0, _len = _ref.length; _i < _len; _i++) {
                a = _ref[_i];
                annotationMap[a.id] = a;
            }
            newData = [];
            for (_j = 0, _len1 = data.length; _j < _len1; _j++) {
                a = data[_j];
                if (annotationMap[a.id]) {
                    annotation = annotationMap[a.id];
                    this.updateAnnotation(annotation, a);
                } else {
                    newData.push(a);
                }
            }
            this.annotations = this.annotations.concat(newData);
            return this.annotator.loadAnnotations(newData.slice());
        };

        StoreEAF.prototype.dumpAnnotations = function() {
            var ann, _i, _len, _ref, _results;
            _ref = this.annotations;
            _results = [];
            for (_i = 0, _len = _ref.length; _i < _len; _i++) {
                ann = _ref[_i];
                _results.push(JSON.parse(this._dataFor(ann)));
            }
            return _results;
        };

        StoreEAF.prototype._dataFor = function(annotation) {
            var data, highlights;
            highlights = annotation.highlights;
            delete annotation.highlights;
            $.extend(annotation, this.options.annotationData);
            data = JSON.stringify(annotation);
            if (highlights) {
                annotation.highlights = highlights;
            }
            return data;
        };

        return StoreEAF;

    })(Annotator.Plugin);

}).call(this);

//
