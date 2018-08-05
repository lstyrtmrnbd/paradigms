;;;; Ch. 8 Symbolic Mathematics: A Simplification Program
;;; Includes the necessary code frome the previous chapter,
;;; but pat-match and rule-based-translator live in "pat-match.lisp"

(defstruct (exp (:type list)
                (:constructor mkexp (lhs op rhs)))
  op lhs rhs)

(defun exp-p (x) (consp x))
(defun exp-args (x) (rest x))

(defun binary-exp-p (x)
  (and (exp-p x) (= (length (exp-args x)) 2)))

(defun prefix->infix (exp)
  "Translate prefix to infix expressions."
  (if (atom exp) exp
      (mapcar #'prefix->infix
              (if (binary-exp-p exp)
                  (list (exp-lhs exp) (exp-op exp) (exp-rhs exp))
                  exp))))

(defparameter *infix->prefix-rules*
  (mapcar #'expand-pat-match-abbrev
          '(((x+ = y+) (= x y))
            ((- x+)    (- x))
            ((+ x+)    (+ x))
            ((x+ + y+) (+ x y))
            ((x+ - y+) (- x y))
            ((x+ * y+) (* x y))
            ((x+ / y+) (/ x y))
            ((x+ ^ y+) (^ x y))))
  "A list of rules, ordered by precedence.")

(defun rule-pattern (rule) (first rule))
(defun rule-response (rule) (second rule))

(defun infix->prefix (exp)
  "Translate an infix expression into prefix notation."
  (cond ((atom exp) exp)
        ((= (length exp) 1) (infix->prefix (first exp)))
        ((rule-based-translator exp *infix->prefix-rules*
                                :rule-if #'rule-pattern :rule-then #'rule-response
                                :action
                                #'(lambda (bindings response)
                                    (sublis (mapcar
                                             #'(lambda (pair)
                                                 (cons (first pair)
                                                       (infix->prefix (rest pair))))
                                             bindings)
                                            response))))
        ((symbolp (first exp))
         (list (first exp) (infix->prefix (rest exp))))
        (t (error "Illegal exp"))))

