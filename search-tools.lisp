;;;; A Set of Searching Tools

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

(defun finite-binary-tree (n)
  "Return a successor function that generates a binary tree with n nodes."
  (lambda (x)
    (remove-if (lambda (child) (> child n))
               (binary-tree x))))

(defun is (value &key (key #'identity) (test #'eql))
  "Returns a predicate that tests for a given value."
  (lambda (path) (funcall test value (funcall key path))))

(defun depth-first-search (start goal-p successors)
  "Search new states first until goal is reached."
  (tree-search (list start) goal-p successors #'append))

(defun prepend (x y) "Prepend y to start of x" (append y x))

(defun breadth-first-search (start goal-p successors)
  "Search old states first until goal is reached."
  (tree-search (list start) goal-p successors #'prepend))

(defun diff (num)
  "Return the function that finds the difference from num."
  (lambda (x) (abs (- x num))))

(defun price-is-right (price)
  "Return a function that measures the difference from price, but gives a big penalty for going over price."
  (lambda (x) (if (> x price)
                  most-positive-fixnum
                  (- price x))))

(defun sorter (cost-fn)
  "Return a combiner function that sorts according to cost-fn."
  (lambda (new old)
    (sort (append new old) #'< :key cost-fn)))

(defun best-first-search (start goal-p successors cost-fn)
  "Search lowest cost states first until goal is reached."
  (tree-search (list start) goal-p successors (sorter cost-fn)))

(defun beam-search (start goal-p successors cost-fn beam-width)
  "Search highest scoring states first until goal is reached, but never consider more than beam-width states at a time."
  (tree-search (list start) goal-p successors
               (lambda (old new)
                 (let ((sorted (funcall (sorter cost-fn) old new)))
                   (if (> beam-width (length sorted))
                       sorted
                       (subseq sorted 0 beam-width))))))

(defun iter-wide-search (start goal-p successors cost-fn &key (width 1) (max 100))
  "Search, increasing beam width from width to max."
  (dbg :search "; Width: ~d" width)
  (unless (> width max)
    (or (beam-search start goal-p successors cost-fn width)
        (iter-wide-search start goal-p successors cost-fn
                          :width (1+ width) :max max))))

(defun graph-search (states goal-p successors combiner
                     &optional (state= #'eql) old-states)
  "Find a state that satisfies goal-p. Start with states, and search according to successsors and combiner. Don't try the same state twice."
  (dbg :search "~&;; Search: ~a" states)
  (cond ((null states) nil)
        ((funcall goal-p (first states)) (first states))
        (t (graph-search
            (funcall combiner
                     (new-states states successors state= old-states)
                     (rest states))
            goal-p successors combiner state=
            (adjoin (first states) old-states
                    :test state=)))))

(defun new-states (states successors state= old-states)
  "Generate successor states that have not been seen before."
  (remove-if #'(lambda (state)
                 (or (member state states :test state=)
                     (member state old-states :test state=)))
             (funcall successors (first states))))

(defun next2 (x) (list (+ x 1) (+ x 2)))

(defun a*-search (paths goal-p successors cost-fn cost-left-fn
                  &optional (state= #'eql) old-paths)
  "Find a path whose state statisfies goal-p. Start with paths, and expand successors, exploring least cost first. When there are duplicate states, keep the one with the lower cost and discard the other."
  (dbg :search ";; Search: ~a" paths)
  (cond
    ((null paths) nil)
    ((funcall goal-p (path-state (first paths)))
     (values (first paths) paths))
    (t (let* ((path (pop paths))
              (state (path-state path)))
         ;; Update PATHS and OLD-PATHS to reflect the new
         ;; successors of STATE
         (setf old-paths (insert-path path old-paths))
         (dolist (state2 (funcall successors state))
           (let* ((cost (+ (path-cost-so-far path)
                           (funcall cost-fn state state2)))
                  (cost2 (funcall cost-left-fn state2))
                  (path2 (make-path :state state2
                                    :previous path
                                    :cost-so-far cost
                                    :total-cost (+ cost cost2)))
                  (old nil))
             ;; Place the new path, path2, in the right list
             (cond
               ((setf old (find-path state2 paths state=))
                (when (better-path path2 old)
                  (setf paths (insert-path path2
                                           (delete old paths)))))
               ((setf old (find-path state2 old-paths state=))
                (when (better-path path2 old)
                  (setf paths (insert-path path2 paths))
                  (setf old-paths (delete old old-paths))))
               (t (setf paths (insert-path path2 paths))))))
         ;; Finally, call A* again with the updated path lists
         (a*-search paths goal-p successors
                    cost-fn cost-left-fn state= old-paths)))))

(defun find-path (state paths state=)
  "Find the path with this state among a list of paths."
  (find state paths :key #'path-state :test state=))

(defun better-path (path1 path2)
  "Is path1 cheaper than path2?"
  (< (path-total-cost path1) (path-total-cost path2)))

(defun insert-path (path paths)
  "Put path into the right position, sorted by total cost."
  (merge 'list (list path) paths #'< :key #'path-total-cost))

(defun path-states (path)
  "Collect the states along this path."
  (if (null path)
      nil
      (cons (path-state path)
            (path-states (path-previous path)))))
