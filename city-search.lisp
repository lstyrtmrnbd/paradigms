(defstruct (city (:type list)) name long lat)

(defparameter *cities*
  '((Atlanta      84.23 33.45) (Los-Angeles   118.15 34.03)
    (Boston       71.05 42.21) (Memphis        90.03 35.09)  
    (Chicago      87.37 41.50) (New-York       73.58 40.47) 
    (Denver      105.00 39.45) (Oklahoma-City  97.28 35.26)
    (Eugene      123.05 44.03) (Pittsburgh     79.57 40.27) 
    (Flagstaff   111.41 35.13) (Quebec         71.11 46.49)
    (Grand-Jct   108.37 39.05) (Reno          119.49 39.30)
    (Houston     105.00 34.00) (San-Francisco 122.26 37.47)
    (Indianapolis 86.10 39.46) (Tampa          82.27 27.57)
    (Jacksonville 81.40 30.22) (Victoria      123.21 48.25)
    (Kansas-City  94.35 39.06) (Wilmington     77.57 34.14)))

(defun neighbors (city)
  "Find all cities within 1000 kilometers."
  (remove-if-not #'(lambda (c)
                   (and (not (eq c city))
                        (< (air-distance c city) 1000.0)))
               *cities*))

(defun city (name)
  "Find the city with this name."
  (assoc name *cities*))

(defun trip (start dest)
  "Search for a way from the start to dest."
  (beam-search start (is dest) #'neighbors
               #'(lambda (c) (air-distance c dest))
               1))

(defconstant earth-diameter 12765.0
  "Diameter of planet earth in kilometers.")

(defun air-distance (city1 city2)
  "The great circle distance between two cities."
  (let ((d (distance (xyz-coords city1) (xyz-coords city2))))
    ;; d is the straight-line chord between two cities,
    ;; the length of the subtending arc is given by:
    (* earth-diameter (asin (/ d 2)))))

(defun xyz-coords (city)
  "Returns the x,y,x coordinates of a point on a sphere. The center is (0 0 0) and the north pole is (0 0 1)."
  (let ((psi (deg->radians (city-lat city)))
        (phi (deg->radians (city-long city))))
    (list (* (cos psi) (cos phi))
          (* (cos psi) (sin phi))
          (sin psi))))

(defun distance (point1 point2)
  "The Euclidean distance between two points. The points are coordinates in n-dimensional space."
  (sqrt (reduce #'+ (mapcar #'(lambda (a b) (expt (- a b) 2))
                            point1 point2))))

(defun deg->radians (deg)
  "Convert degrees and minutes to radians."
  (* (+ (truncate deg) (* (rem deg 1) 100/60)) pi 1/180))



     
