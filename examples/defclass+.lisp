(in-package :core-server)

;; +-------------------------------------------------------------------------
;; | Defining Classes with defclass+
;; +-------------------------------------------------------------------------
(defclass+ a ()
  ((slot-in-the-server :host local :initform "I can win")
   (slot-in-the-client :host remote :initform (jobject :foo "bar"
						       :moo "tar")))
  (:ctor make-a))

;; { 
;;   "coreClass": { 
;;     "slotInTheClient": { 
;;       "name": 'slotInTheClient', 
;;       "type": 'primitive', 
;;       "label": 'Slot In The Client' }
;;      }
;;   , 
;;   "slotInTheClient": { 
;;     "foo": 'bar', 
;;     "moo": 'tar' }
;;    }
(let ((a (make-a :slot-in-the-server "win can I")))
  (with-js (a) (make-indented-stream *core-output*)
    a))


;; +-------------------------------------------------------------------------
;; | Defining Components with defcomponent
;; +-------------------------------------------------------------------------
(defcomponent b (a)
  ()
  (:ctor make-b))


(defmethod/local get-time ((self b))
  (get-universal-time))

(defmethod/remote init ((self b))
  (alert (+ "Hello, i'm B, time is :"
	    (date-to-string
	     (list-date-to-javascript (get-time self))))))

(let ((b (make-b :slot-in-the-server "win can I")))
  (with-js (b) (make-indented-stream *core-output*)
    b))
;;function (toExtend, k11) {
;;   var k11 = k11 || window.k;
;;   var g2 = toExtend || new Object();
;;   extend({
;;     clientDestroy: makeMethod(function (k1108) {
;;       var self = self || this;
;;       self._destroy = function (k1114) {
;;         return k1114(this);
;;       };
;;       return self.destroy(k1108);
;;     }),
;;     _destroy: makeMethod(function (k1100) {
;;       var self = self || this;
;;       addToGc(function (k1104) {
;;         return k1104(new Array('TEST-COMPONENT-DESTROY.core' + '?s:' + 'unbound-session-id', 'b-YiGBSxYe'));
;;       });
;;       return k1100(true);
;;     }),
;;     funkall: makeMethod(function (action, args, k191) {
;;       var k191 = k191 || window.k;
;;       var self = self || this;
;;       return funcallCc(self.url + action + '$', args, function (g93) {
;;         if ('function' == (typeof g93)) {
;;           return g93(self, k191);
;;         } else {
;;           return k191(g93);
;;         };
;;       });
;;     }),
;;     getTime: function (k187) {
;;       var self = self || this;
;;       return self.funkall('?s:' + 'unbound-session-id' + '$k:' + 'b-YiGBSxYe' + '$method:' + 'GET-TIME', {}, k187);
;;     }
;;   }, g2);
;;   mapobject(function (k, v, k158) {
;;     var k158 = k158 || window.k;
;;     if ((!(('undefined' == (typeof v)) || (null === v)) && (('' === g2[k]) || (('undefined' == (typeof g2[k])) || (null === g2[k])) || ('undefined' === g2[k]))) || ('undefined' === (typeof g2[k]))) {
;;       return k158(g2[k] = v);
;;     } else {
;;       return k158(null);
;;     };
;;   }, {
;;     slotInTheClient: { 
;;       "foo": 'bar', 
;;       "moo": 'tar' }
;;     ,
;;     url: null
;;   });
;;   g2.ctor = arguments.callee;
;;   return apply(makeMethod(function (k147) {
;;     var self = self || this;
;;     return self.getTime(function (g52) {
;;       return k147(alert('Hello, i\'m B, time is :' + dateToString(listDateToJavascript(g52))));
;;     });
;;   }), g2, null, function (value9) {
;;     g2.destroy = composeProg1Cc(makeMethod(function (k128) {
;;       var self = self || this;
;;       var g34 = self._destroy;
;;       if (!(('undefined' == (typeof g34)) || (null === g34))) {
;;         return self._destroy(function (value37) {
;;           removeSlots(self, new Array('_destroy'));
;;           removeSlots(self, new Array('destroy'));
;;           return k128(self);
;;         });
;;       } else {
;;         removeSlots(self, new Array('destroy'));
;;         return k128(self);
;;       };
;;     }), g2.destroy);
;;     g2._destroy = makeMethod(function (k115) {
;;       var self = self || this;
;;       addToGc(function (k119) {
;;         return k119(new Array('TEST-COMPONENT-DESTROY.core' + '?s:' + 'unbound-session-id', 'b-YiGBSxYe'));
;;       });
;;       return k115(true);
;;     });
;;     return k11(g2);
;;   });
;; }
(let ((b (make-b :slot-in-the-server "win can I")))
  (with-js (b) (make-indented-stream *core-output*)
    b))


;; +-------------------------------------------------------------------------
;; | Inheritance
;; +-------------------------------------------------------------------------
(defcomponent c (b)
  ()
  (:ctor make-c))

(defmethod/remote init ((self c))
  (call-next-method self)
  (alert (+ "I am C.")))

;; function (toExtend, k11) {
;;   var k11 = k11 || window.k;
;;   var g2 = toExtend || new Object();
;;   extend({
;;     clientDestroy: makeMethod(function (k1113) {
;;       var self = self || this;
;;       self._destroy = function (k1119) {
;;         return k1119(this);
;;       };
;;       return self.destroy(k1113);
;;     }),
;;     _destroy: makeMethod(function (k1105) {
;;       var self = self || this;
;;       addToGc(function (k1109) {
;;         return k1109(new Array('TEST-COMPONENT-DESTROY.core' + '?s:' + 'unbound-session-id', 'c-NHVQvjPb'));
;;       });
;;       return k1105(true);
;;     }),
;;     funkall: makeMethod(function (action, args, k196) {
;;       var k196 = k196 || window.k;
;;       var self = self || this;
;;       return funcallCc(self.url + action + '$', args, function (g98) {
;;         if ('function' == (typeof g98)) {
;;           return g98(self, k196);
;;         } else {
;;           return k196(g98);
;;         };
;;       });
;;     }),
;;     getTime: function (k192) {
;;       var self = self || this;
;;       return self.funkall('?s:' + 'unbound-session-id' + '$k:' + 'c-NHVQvjPb' + '$method:' + 'GET-TIME', {}, k192);
;;     }
;;   }, g2);
;;   mapobject(function (k, v, k163) {
;;     var k163 = k163 || window.k;
;;     if ((!(('undefined' == (typeof v)) || (null === v)) && (('' === g2[k]) || (('undefined' == (typeof g2[k])) || (null === g2[k])) || ('undefined' === g2[k]))) || ('undefined' === (typeof g2[k]))) {
;;       return k163(g2[k] = v);
;;     } else {
;;       return k163(null);
;;     };
;;   }, {
;;     slotInTheClient: { 
;;       "foo": 'bar', 
;;       "moo": 'tar' }
;;     ,
;;     url: null
;;   });
;;   g2.ctor = arguments.callee;
;;   return apply(makeMethod(function (k147) {
;;     var self = self || this;
;;     return self.getTime(function (g57) {
;;       alert('Hello, i\'m B, time is :' + dateToString(listDateToJavascript(g57)));
;;       return k147(alert('I am C.'));
;;     });
;;   }), g2, null, function (value9) {
;;     g2.destroy = composeProg1Cc(makeMethod(function (k128) {
;;       var self = self || this;
;;       var g34 = self._destroy;
;;       if (!(('undefined' == (typeof g34)) || (null === g34))) {
;;         return self._destroy(function (value37) {
;;           removeSlots(self, new Array('_destroy'));
;;           removeSlots(self, new Array('destroy'));
;;           return k128(self);
;;         });
;;       } else {
;;         removeSlots(self, new Array('destroy'));
;;         return k128(self);
;;       };
;;     }), g2.destroy);
;;     g2._destroy = makeMethod(function (k115) {
;;       var self = self || this;
;;       addToGc(function (k119) {
;;         return k119(new Array('TEST-COMPONENT-DESTROY.core' + '?s:' + 'unbound-session-id', 'c-NHVQvjPb'));
;;       });
;;       return k115(true);
;;     });
;;     return k11(g2);
;;   });
;; }
(let ((c (make-c :slot-in-the-server "win can I")))
  (with-js (c) (make-indented-stream *core-output*)
    c))

(let ((b (make-b :slot-in-the-server "win can I")))
  (with-js (b) (make-indented-stream *core-output*)
    (with-call/cc
      (call/cc b
	       (extend (jobject :slot-in-the-client "remote!")
		       (<:input :type "text" :name "field1"))))))