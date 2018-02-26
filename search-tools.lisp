;;;; A Set of Searching Tools

(defvar *dbg-ids* nil "Identifiers used by dbg")

(defun dbg (id format-string &rest args)
  "Print debugging info if (DEBUG ID) has been specified."
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (apply #'format *debug-io* format-string args)))

(defun dbg-indent (id indent format-string &rest args)
  "Print indented debugging info if (DEBUG ID) has been specified."
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (dotimes (i indent) (princ "  " *debug-io*))
    (apply #'format *debug-io* format-string args)))

(defun debug (&rest ids)
  "Start dbg output on the given ids."
  (setf *dbg-ids* (union ids *dbg-ids*)))

(defun undebug (&rest ids)
  "Stop dbg on the ids. With no ids, stop dbg altogether."
  (setf *dbg-ids* (if (null ids) nil
                      (set-difference *dbg-ids* ids))))

(defun tree-search (states goal-p successors combiner)
  "Find a state that satisfies goal-p. Start with states and search according to successors and combiner."
  (dbg :search "~&;; Search: ~a" states)
  (cond ((null states) nil)
        ((funcall goal-p (first states)) (first states))
        (t (tree-search
             (funcall combiner
                      (funcall successors (first states))
                      (rest states))
             goal-p successors combiner))))

(defun binary-tree (x) (list (* 2 x) (+ 1 (* 2 x))))

(defun quad-tree (x) (list (* 4 x) (+ 1 (* 4 x)) (+ 2 (* 4 x)) (+ 3 (*  4 x))))

(defun finit-binary-tree (n)
  "Return a successor function that generates a binary tree with n nodes."
  (lambda (x)
    (remove-if (lambda (child) (> child n))
               (binary-tree))))

(defun is (value) #'(lambda (x) (eql x value)))

(defun depth-first-search (start goal-p successors)
  "Search new states first until goal is reached."
  (tree-search (list start) goal-p successors #'append))

(defun prepend (x y) "Prepend y to start of x" (append y x))

(defun breadth-first-search (start goal-p successors)
  "Search old states first until goal is reached."
  (tree-search (list start) goal-p successors #'prepend))
