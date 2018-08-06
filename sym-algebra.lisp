;;;; Ch. 8 Symbolic Mathematics: A Simplification Program
;;; Includes the necessary code frome the previous chapter,
;;; but pat-match and rule-based-translator live in "pat-match.lisp"

(defstruct (rule (:type list)) pattern response)

(defun rule-pattern (rule) (first rule))
(defun rule-response (rule) (second rule))

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


;; redefinition of function in "pat-match.lisp"
(defun variable-p (exp)
  "Variables are only the symbols M thru Z."
  (member exp '(x y z m n o p q r s t u v w))) ; x y z first to match quicker

(pat-match-abbrev 'x+ '(?+ x))
(pat-match-abbrev 'y+ '(?+ y))

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

(defparameter *simplification-rules*
  (mapcar #'infix->prefix
          '((x + 0 = x)
            (0 + x = x)
            (x + x = 2 * x)
            (x - 0 = x)
            (0 - x = - x)
            (x - x = 0)
            (- - x = x)
            (x * 1 = x)
            (1 * x = x)
            (x * 0 = 0)
            (0 * x = 0)
            (x * x = x ^ 2)
            (x / 0 = undefined)
            (0 / x = 0)
            (x / 1 = x)
            (x / x = 1)
            (0 ^ 0 = undefined)
            (x ^ 0 = 1)
            (0 ^ x = 0)
            (1 ^ x = 1)
            (x ^ 1 = x)
            (x ^ -1 = 1 / x)
            (x * (y / x) = y)
            ((y / x) * x = y)
            ((y * x) / x = y)
            ((x * y) / x = y)
            (x + - x = 0)
            ((- x) + x = 0)
            (x + y - x = y))))

(defun ^ (x y) "Exponentiation" (expt x y))
