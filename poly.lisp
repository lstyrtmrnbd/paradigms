(proclaim '(inline main-var degree coef
	    var= var> poly make-poly))

(deftype polynomial () 'simple-vector)

(defun main-var (p) (svref (the polynomial p) 0))
(defun coef (p i)   (svref (the polynomial p) (+ i 1)))
(defun degree (p)   (- (length (the polynomial p)) 2))

(defun poly (x &rest coefs)
  "Make a polynomial with main variable x 
  and coefficients in increasing order."
  (apply #'vector x coefs))

(defun make-poly (x degree)
  "Make the polynomial 0 + 0*x + 0*x^2 + ... + 0*x^degree"
  (let ((p (make-array (+ degree 2) :initial-element 0)))
    (setf (main-var p) x)
    p))

(defsetf main-var (p) (val)
  `(setf (svref (the polynomial ,p) 0) ,val))

(defsetf coef (p i) (val)
  `(setf (svref (the polynomial ,p) (+ ,i 1)) ,val))

(defun prefix->canon (x)
  "Convert a prefix Lisp expression to canonical form.
  Exs: (+ (^ x 2) (* 3 x)) => #(x 0 3 1)
       (- (* (- x 1) (+ x 1)) (- (^ x 2) 1)) => 0"
  (cond ((numberp x) x)
	((symbolp x) (poly x 0 1))
	((and (exp-p x) (get (exp-op x) 'prefix->canon))
	 (apply (get (exp-op x) 'prefix->canon)
		(mapcar #'prefix->canon (exp-args x))))
	(t (error "Not a polynomial: ~a" x))))

(dolist (item '((+ poly+) (- poly-) (* poly*poly)
		(^ poly^) (D deriv-poly)))
  (setf (get (first item) 'prefix->canon) (second item)))

(defun poly+ (&rest args)
  "Unary or binary polynomial addition."
  (ecase (length args)
    (1 (first args))
    (2 (poly+poly (first args) (second args)))))

(defun poly- (&rest args)
  "Unary or binary polynomial subtraction."
  (ecase (length args)
    (1 (poly*poly -1 (first args)))
    (2 (poly+poly (first args) (poly*poly -1 (second args))))))

(defun var= (x y) (eq x y))
(defun var> (x y) (string> x y))

(defun poly+poly (p q)
  "Add two polynomials."
  (normalize-poly
   (cond
     ((numberp p)                      (k+poly p q))
     ((numberp q)                      (k+poly q p))
     ((var= (main-var p) (main-var q)) (poly+same p q))
     ((var> (main-var q) (main-var p)) (k+poly q p))
     (t                                (k+poly p q)))))

(defun k+poly (k p)
  "Add a constant k to a polynomial p."
  (cond ((eql k 0) p)
	((and (numberp k) (numberp p))
	 (+ k p))
	(t (let ((r (copy-poly p)))    ;Add k to x^0 term of p
	     (setf (coef r 0) (poly+poly (coef r 0) k))
	     r))))

(defun poly+same (p q)
  "Add two polynomials with the same variable."
  ;;First assure that q is the higher degree polynomial
  (if (> (degree p) (degree q))
      (poly+same q p)
      ;;Add each element of p into r (a copy of q).
      (let ((r (copy-poly q)))
	(loop for i from 0 to (degree p) do
	     (setf (coef r i) (poly+poly (coef r i) (coef p i))))
	r)))

(defun copy-poly (p)
  "Make a copy of a polynomial."
  (copy-seq p))

(defun poly*poly (p q)
  "Multiply two polynomials."
  (normalize-poly
   (cond
     ((numberp p)                      (k*poly p q))
     ((numberp q)                      (k*poly q p))
     ((var= (main-var p) (main-var q)) (poly*same p q))
     ((var> (main-var q) (main-var p)) (k*poly q p))
     (t                                (k*poly p q)))))

(defun k*poly (k p)
  "Multiply a polynomial p by a constant factor k."
  (cond
    ((eql k 0) 0)
    ((eql k 1) p)
    ((and (numberp k)
	  (numberp p))
     (* k p)) ; Multiply numbers
    (t ; Multiply each coefficient
     (let ((r (make-poly (main-var p) (degree p))))
       ;; Accumulate result in r; r[i] = k*p[i]
       (loop for i from 0 to (degree p) do
	    (setf (coef r i) (poly*poly k (coef p i))))
       r))))

(defun poly*same (p q)
  "Multiply two polynomials with the same variable."
  ;; r[i] = p[0]*q[i] + p[1]*q[i-1] + ...
  (let* ((r-degree (+ (degree p) (degree q)))
	 (r (make-poly (main-var p) r-degree)))
    (loop for i from 0 to (degree p) do
	 (unless (eql (coef p i) 0)
	   (loop for j from 0 to (degree q) do
		(setf (coef r (+ i j))
		      (poly+poly (coef r (+ i j))
				 (poly*poly (coef p i)
					    (coef q j)))))))
    r))

(defun normalize-poly (p)
  "Alter a polynomial by dropping trailing zeros."
  (if (numberp p)
      p
      (let ((p-degree (- (position 0 p :test (complement #'eql)
				   :from-end t)
			 1)))
	(cond ((<= p-degree 0) (normalize-poly (coef p 0)))
	      ((< p-degree (degree p))
	       (delete 0 p :start p-degree))
	      (t p)))))

(defun poly^n (p n)
  "Raise polynomial p to the nth power, n>=0."
  (check-type n (integer 0 *))
  (cond ((= n 0) (assert (not (eql p 0))) 1)
	((integerp p) (expt p n))
	(t (poly*poly p (poly^n p (- n 1))))))
