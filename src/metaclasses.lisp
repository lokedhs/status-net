(in-package :status-net)

(defclass atom-entity-class (standard-class)
  ())

(defclass atom-entity ()
  ())

(defmethod closer-mop:validate-superclass ((class atom-entity-class) (superclass standard-object))
  t)

(defclass atom-entity-class-slot-definition-mixin ()
  ((xpath               :type (or null string)
                        :initform nil
                        :initarg :xpath
                        :accessor atom-slot/xpath)
   (xpath-default-value :initform nil
                        :initarg :xpath-default-value
                        :accessor atom-slot/xpath-default-value)
   (node-parser         :type (or null symbol function)
                        :initform nil
                        :initarg :node-parser
                        :accessor atom-slot/node-parser)))

(defclass atom-entity-class-direct-slot-definition (atom-entity-class-slot-definition-mixin
                                                    closer-mop:standard-direct-slot-definition)
  ())

(defclass atom-entity-class-effective-slot-definition (atom-entity-class-slot-definition-mixin
                                                       closer-mop:standard-effective-slot-definition)
  ())

(defmethod closer-mop:direct-slot-definition-class ((class atom-entity-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'atom-entity-class-direct-slot-definition))

(defmethod closer-mop:effective-slot-definition-class ((class atom-entity-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'atom-entity-class-effective-slot-definition))

(defun ensure-slot-value (instance field-name &optional default-value)
  "Returns the value of slot FIELD-NAME in INSTANCE. If the slot is unbound, return DEFAULT-VALUE."
  (if (and (slot-exists-p instance field-name)
           (slot-boundp instance field-name))
      (slot-value instance field-name)
      default-value))

(defmethod closer-mop:compute-effective-slot-definition ((class atom-entity-class) slot-name direct-slots)
  (let ((result (call-next-method)))
    (setf (atom-slot/xpath result) (ensure-slot-value (car direct-slots) 'xpath nil))
    (setf (atom-slot/xpath-default-value result) (ensure-slot-value (car direct-slots) 'xpath-default-value nil))
    (setf (atom-slot/node-parser result) (ensure-slot-value (car direct-slots) 'node-parser nil))
    result))

(defun remove-keyword-from-list (arg-list keyword)
  (when arg-list
    (nconc (unless (eq (car arg-list) keyword)
             (list (car arg-list) (cadr arg-list)))
           (remove-keyword-from-list (cddr arg-list) keyword))))



(macrolet ((init-reinit (name)
             `(defmethod ,name :around ((class atom-entity-class)
                                        &rest args
                                        &key direct-superclasses)
                #+nil(let ((root-class (find-class 'atom-entity)))
                  (cond ((or (equal class root-class)
                             (member root-class direct-superclasses))
                         (call-next-method))
                        (t
                         (apply #'call-next-method class
                                :direct-superclasses (append (list root-class) direct-superclasses)
                                (remove-keyword-from-list args :direct-superclasses)))))
                ;; Class-level initialisations
                (declare (ignore args direct-superclasses))
                (call-next-method))))
  (init-reinit initialize-instance)
  (init-reinit reinitialize-instance))
