;; Note: Only works if unicode is supported in symbols, due to espeanto's character
;; set.
(defpackage :esperanto
  (:use :cl :utils)
  (:nicknames :esp))

(in-package :esperanto)

(assert (unwind-protect (code-char  (1- 1114112)) nil))

(defun inside (from into)
  (if (eq into from)
      into
      (labels ((inside-aux (from into inclusive exclusive stop)
		 (if (= exclusive stop)
		     nil
		     (if (equalp (subseq from inclusive exclusive)
				 into)
			 into
			 (inside-aux from into (1+ inclusive) (1+ exclusive) stop)))))
	(inside-aux from into 0 (length into) (1+ (length from))))))

(defun concatenate-list (list)
  (reduce (lambda (x y) (concatenate 'string x y)) list))

(defvar *rules* nil
  "Assign each defrule here so they can be searched.")

(defvar *terminals* nil
  "Alist of terminals classes.")

(defun defterminals (class-name class-list)
  "Define a terminals class."
  (dolist (term class-list)
    (setf (symbol-plist term) (list 'terminal t)))
  (setf *terminals*
	(cons (list class-name class-list)
	      *terminals*)))

(defun terminal? (sym)
  "Is the symbol a terminal symbol?"
  (and (symbolp sym)
       (equal (symbol-plist sym) (list 'terminal t))))

(defun terminal-class? (class)
  "Determine if a class is a terminal class."
  (some #'(lambda (x)
	    (eql class (car x))) *terminals*))

(defun token? (class-item)
  (when (symbolp class-item)
    (member class-item (list 'unicode-pua-2 'unicode-pua-1 'unicode-pua-3 ''unicode-pua-4))))

(defun token-class? (list)
  "Determine if the class is tokenized (as opposed to being a terminal-class)."
  (and (listp list)
       (some #'token? list)))

(defun subterminal? (rule)
  (and (consp rule)
       (some #'(lambda (item)
		 (eq (car rule)
         item)) (list '^ '+ 'g '*))
       (every #'terminal-class? (cdr rule))))

;; Notation:
;; Note that this is essentially a lispy BNF.
;; (+ ...) means 1 or more. NIL on failure.
;; (* ...) means 0 or more. Empty string on failure
;; (^ ...) means OR, so like a short circuit +. NIL on failure.
;; (g ...) is a grouping. Like a hybrid between logAND and concatenation.
;; There is an implicit (g ...) in defrule, so (defrule <rulename> (g ...))
;; defterminals are always of the form (defterminals <class> (<terminal> ...))


;; Compile-time functions for rules
(defun expand-terminal (class)
  "Get the expansion of a terminal class."
  (some #'(lambda (x)
	    (if (eql class (car x))
		(cadr x))) *terminals*))

(defun append-each (lists &optional acc)
  (if (null lists)
      acc
      (append-each (cdr lists) (append acc (car lists)))))

(defmacro mappend (function list &rest more-lists)
  `(append-each (mapcar ,function ,list ,@more-lists)))

(defmacro exp-aux (args)
  "Auxillary function for ex-X macros."
  (with-gensyms (arg)
    `(mappend #'(lambda (,arg)
		 (cond ((terminal-class? ,arg) (expand-terminal ,arg))
		       (t (list ,arg)))) ,args)))
;; These use weird unicode symbols as a gensymmy object code. This could be done
;; better. Also, if an input word is NIL everything breaks.

(defmacro exp-^ (args)
  "Given forms 1 2 3 ... in (^ 1 2 3 ...) expand it."
  `(cons 'unicode-pua-1 (exp-aux ,args)))

(defmacro exp-+ (args)
  "Expand (+ 1 2 3 ...)"
  `(cons 'unicode-pua-4 (exp-aux ,args)))

(defmacro exp-* (args)
  "Expand (* 1 2 3 ...)"
  `(cons 'unicode-pua-3 (exp-aux ,args)))

(defmacro exp-g (args)
  "Expand (* 1 2 3 ...)"
  `(cons 'unicode-pua-2 (exp-aux ,args)))

(defmacro defmatch (name symbol)
  "Create a true predicate to determine an expanded class's token."
  (with-gensyms (expanded)
    `(defpredicate ,name ,(list expanded)
       (and (listp ,expanded)
	    (and (symbolp (car ,expanded))
		 (eq (car ,expanded) ',symbol))))))

(defmatch ^? unicode-pua-1)
(defmatch g? unicode-pua-2)
(defmatch *? unicode-pua-3)
(defmatch +? unicode-pua-4)

(defmacro expand-correct (args)
  "Auxillary macro for expand. Used for subterminal (operation terminal terminal...)
   argument."
  (with-gensyms (once)
    `(let ((,once ,args))
       ;; Cond necessary because macros cannot be funcall'd
       (cond ((eql '^ (car ,once)) (exp-^ (cdr ,once)))
	     ((eql '+ (car ,once)) (exp-+ (cdr ,once)))
	     ((eql '* (car ,once)) (exp-* (cdr ,once)))
	     ((eql 'g (car ,once)) (exp-g (cdr ,once)))))))

(defun rule-class? (name)
  (some #'(lambda (x)
	    (eql (car x) name)) *rules*))

(defun read-expansion (rule)
  (labels ((read-expansion-aux (rules*)
	     (if (null rules*)
		 nil
		 (if (eq rule (caar rules*))
		     (cadar rules*)
		     (read-expansion-aux (cdr rules*))))))
    (read-expansion-aux *rules*)))

(defun expanded-p (rule)
  (not (null (read-expansion rule))))

(defun expand (rule)
  "Expand a rule."
  (cond ((null rule) nil)
	((rule-class? rule) (read-expansion rule))
	((terminal-class? rule)
	 (expand-terminal rule))
	((subterminal? rule)
	 (expand-correct rule))
	(t (expand-correct (cons (car rule)
				 (mapcar #'expand (cdr rule)))))))

(defun make-a-rule (name expansion)
  "Auxilliary function to defrule."
  (if (expanded-p name)
      nil
       (setf *rules*
	     (cons (list name expansion)
		   *rules*))))

(defmacro defrule (name &rest expansion)
  `(make-a-rule ',name (expand '(g ,@expansion))))

(defrule suffix (^ noun-suffix adverb-suffix adjective-suffix verb-suffix
		   oddball-suffixes))

;; English explaination of a polyword:
;; 1) Search if there is a prefix. If there isn't, it is optional, so move on.
;; 2) Search if the term is a member of the root terminal class. If it isn't,
;;    just return it. This will make any subsequent search be looking at an
;;    empty string "".
;; 3) Search if the last part of the term is 1 or more presuffixes followed by
;;    one or more suffixes. If not, search if it is one or more suffixes. If not
;;    , return the search term.
;; When the above returns a match, it will look like (class . "match"). When it
;; is forced to return raw data, it just looks like "$", where $ is the data.
;; The result is contained in a list, like ((class . "match") "$")

(defrule polyword (* prefix) (^ root) (^ (g (+ presuffix)
					    (+ suffix))
					 (+ suffix)))

(defrule correlative correlative-prefix correlative-suffix)

(defun match-terminal? (word terminal)
  "Partial predicate which returns whether the word matches the terminal symbol. 
   Returns either the match or NIL."
  (awhen (inside (symbol-name word) (symbol-name terminal))
    (intern it)))

(defun match-terminal-class? (word terminal-class)
  "Partial predicate built over match-terminal?"
  (some #'(lambda (terminal)
	    (match-terminal? word terminal)) terminal-class))

(defun operation (word class)
  "Run the function operation for each class type."
  (cond ((null class) 'unicode-pua-3)
	((atom class) (match-terminal? word class))
	((g? class)
	 (symb (operation (cadr class)) (operation (cddr class))))
	((^? sym)
	 )
	((*? sym))
	((+? sym))
	(t )))
