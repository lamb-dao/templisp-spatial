;;;; merge csv files for masters data

;;;; ===================================  set environment
                                        ; imports
(ql:quickload :click)
(ql:quickload :lisp-stat)
(ql:quickload :plot/vega)

                                        ; package def
(defpackage :table-ops
  (:use :cl :lisp-stat :click)
  (:shadowing-import-from :select :which))

                                        ; enter package
(in-package :table-ops) ; Also enter this in the REPL!

;;;; ===================================  load file paths

;; (makunbound '*data-path*)
;; (makunbound '*data-files*)
                                        ; defvar gets one set!
(defvar *data-path* (merge-pathnames "db/1/masters/data/" (user-homedir-pathname)))

                                        ;ls a list of pathnames
(uiop:with-current-directory (*data-path*)
  (defvar *data-files* (uiop:directory-files "./"))
  (format t "~{~a~%~}" *data-files*))

;;;; ===================================  get mean height from G2 and G6
(defdf *guardrows-height* (read-csv (merge-pathnames "guardRows_height.csv" *data-path*)))

                                        ; add type, unit, label
(heuristicate-types *guardrows-height*)
(set-properties *guardrows-height* :type '(:sample :categorical))
(set-properties *guardrows-height* :unit '(:height-cm cm
                                           :sample :NA))
(set-properties *guardrows-height* :label '(:height-cm "Photographed height at second highest collar of the rachis"
                                            :sample "Sample Name"))
(describe *guardrows-height*)
                                        ; split into  two df where sample contains G2-, G1-
(defdf *G2* (filter-rows *guardrows-height* '(str:containsp "G2-" sample)))
(defdf *G6* (filter-rows *guardrows-height* '(str:containsp "G6-" sample)))
                                        ; check each has 20 samples
(assert (= 20
           (count-rows *G2* 'height-cm #'(lambda (x) (< 0 x)))
           (count-rows *G6* 'height-cm #'(lambda (x) (< 0 x)))))

                                        ; take mean
(def *g2-mean-height* (round (mean *g2*:height-cm)))
(def *g6-mean-height* (round (mean *g6*:height-cm)))
(variables)

                                        ; clean
(show-data-frames)
(undef *G6* *G2* *guardrows-height*)

;;;; ===================================  get mean weight, dia from G2 and G6
(defdf *guardRows-weight-dia* (read-csv (merge-pathnames "guardRows_weight_dia.csv" *data-path*)))
                                        ; add type, unit, label
(heuristicate-types *guardrows-weight-dia*)
(set-properties *guardrows-weight-dia* :type '(:sample :categorical))
(set-properties *guardrows-weight-dia* :unit '(:weight-g-bag g
                                               :dia-mm-ten mm))
(set-properties *guardrows-weight-dia* :label '(:weight-g-bag "Weight including bag"
                                                :dia-mm-ten "diameter including 10mm overage"))
(describe *guardrows-weight-dia*)

                                        ; split into two df where sample contains G2-, G1-
(defdf *G2* (filter-rows *guardrows-weight-dia* '(str:containsp "G2-" sample)))
(defdf *G6* (filter-rows *guardrows-weight-dia* '(str:containsp "G6-" sample)))
                                        ; check each has 20 samples
(assert (= 20
           (count-rows *G2* 'dia-mm-ten #'(lambda (x) (< 0 x)))
           (count-rows *G6* 'dia-mm-ten #'(lambda (x) (< 0 x)))))
                                        ; take means
(defun sig-fig (places float)
  "truncates float to sig-fig places"
  (assert (>= places 0))
  (let ((place-maker (expt 10 places)))
    (/ (round (* float place-maker)) place-maker)))

;; G2 weight
(def *g2-mean-weight-raw* (sig-fig 2 (mean *g2*:weight-g-bag)))
;; dia
(def *g2-mean-diameter-raw* (sig-fig 2 (mean *g2*:dia-mm-ten)))
;; G6 weight
(def *g6-mean-weight-raw* (sig-fig 2 (mean *g6*:weight-g-bag)))
;; dia
(def *g6-mean-diameter-raw* (sig-fig 2 (mean *g6*:dia-mm-ten)))
(variables)

                                        ; clean
(show-data-frames)
(undef *G2* *G6* *guardrows-weight-dia*)

;;;; ===================================; merge for special plots 1001-1010
                                        ; open and label files
(defdf *plotSpecial-height* (read-csv (merge-pathnames "plotSpecial_height.csv" *data-path*)))
(heuristicate-types *plotspecial-height*)
(set-properties *plotspecial-height* :type '(:sample :categorical
                                             :name :categorical))
(set-properties *plotspecial-height* :unit '(:height-cm :cm))
(set-properties *plotspecial-height* :label '(:height-cm "Photographed height at second highest collar of the rachis"
                                              :name "name of the special category of segment"))
(describe *plotspecial-height*)

(defdf *plotSpecial-weight-dia* (read-csv (merge-pathnames "plotSpecial_weight_dia.csv" *data-path*)))
(heuristicate-types *plotspecial-weight-dia*)
(set-properties *plotspecial-weight-dia* :type '(:sample :categorical))
(set-properties *plotspecial-weight-dia* :unit '(:weight-g-bag g
                                                 :dia-mm-ten mm))
(set-properties *plotspecial-weight-dia* :label '(:weight-g-bag "Weight including bag"
                                                  :dia-mm-ten "diameter including 10mm overage"
                                                  :x "place holder for name"))
(describe *plotspecial-weight-dia*)


                                        ; merge on sample column
;; ensure sample col is identical
(assert (equalp (column *plotspecial-height* 'sample)
                (column *plotspecial-weight-dia* 'sample)))
;; add and name columns
(defdf *plotspecial-all*
    (add-columns *plotspecial-height*
                 'weight-g-bag *plotspecial-weight-dia*:weight-g-bag
                 'dia-mm-ten *plotspecial-weight-dia*:dia-mm-ten))
(describe *plotspecial-all*)
                                        ; clean
(show-data-frames)
(undef *plotspecial-height* *plotspecial-weight-dia*)


;;;; ===================================; merge for plots 1-1000

(defdf *plot-height* (read-csv (merge-pathnames "plot_height.csv" *data-path*)))
(heuristicate-types *plot-height*)

(set-properties *plot-height* :type '(:sample :categorical))
(set-properties *plot-height* :unit '(:height-cm :cm))
(set-properties *plot-height* :label '(:height-cm "Photographed height at second highest collar of the rachis"))
(describe *plot-height*)

(defdf *plot-weight-dia* (read-csv (merge-pathnames "plot_weight_dia.csv" *data-path*)))
(heuristicate-types *plot-weight-dia*)
(set-properties *plot-weight-dia* :type '(:sample :categorical
                                          :field-status :categorical
                                          :bin-status :categorical))
(set-properties *plot-weight-dia* :unit '(:weight-g-bag g
                                                 :dia-mm-ten mm))
(set-properties *plot-weight-dia* :label '(:weight-g-bag "Weight including bag"
                                                  :dia-mm-ten "diameter including 10mm overage"
                                                  :x "place holder for name"))
(describe *plot-weight-dia*)

                                        ; merge on sample column
;; ensure sample col is identical
(assert (equalp (column *plot-height* 'sample)
                (column *plot-weight-dia* 'sample)))
;; add and name columns
(defdf *plot-all*
    (add-columns *plot-height*
                 'weight-g-bag *plot-weight-dia*:weight-g-bag
                 'dia-mm-ten *plot-weight-dia*:dia-mm-ten
                 'bin-status *plot-weight-dia*:bin-status
                 'field-status *plot-weight-dia*:field-status))

(describe *plot-all*)
; clean
(show-data-frames)
(undef *plot-weight-dia* *plot-height*)

;;;; ===================================; column bind phenotype

(defdf *phenotype* (read-csv (merge-pathnames "phenotype.csv" *data-path*)))
(heuristicate-types *phenotype*)
(set-properties *phenotype* :type '(:plot :categorical
                                    :name :categorical
                                    :row-type :categorical
                                    :hulless-condition :categorical
                                    :sblotch-rating :categorical))
(set-properties *phenotype* :label '(:plot "equivalent to sample in other dataframes"
                                     :name "string representation of line"
                                     :row-type "2 or 6 row barley also [w]heat or [d]urum"
                                     :hulless-condition "[c]overed or [h]ulless also [w]heat or [d]urum"
                                     :sblotch-rating "rated intensity of spot blotch"))
(describe *phenotype*)
(keys *phenotype*)
;; => #(PLOT EXPT ENTRY NAME ALIAS BLOCK ROW-TYPE HULLESS-CONDITION SBLOTCH-RATING)
                                        ; merge on sample column
;; ensure column to bind on is identical
(assert (equalp (column *phenotype* 'plot)
                (column *plot-all* 'sample)))

(add-columns! *plot-all*
              'name *phenotype*:name
              'row-type *phenotype*:row-type
              'hulless-condition *phenotype*:hulless-condition
              'sblotch-rating *phenotype*:sblotch-rating)


;;;; ===================================; row append 1-1010

                                        ; check columns are matched
(keys *plot-all*)
;; #(SAMPLE HEIGHT-CM NAME WEIGHT-G-BAG DIA-MM-TEN BIN-STATUS FIELD-STATUS ROW-TYPE HULLESS-CONDITION SBLOTCH-RATING)
(keys *plotspecial-all*)
;; #(SAMPLE HEIGHT-CM NAME WEIGHT-G-BAG DIA-MM-TEN)
;; needs: bin-status field-status row-type hulless-condition sblotch-rating
;; '(sample name height-cm weight-g-bag dia-mm-ten bin-status field-status row-type hulless-condition sblotch-rating)
                                        ; row append plotSpecial values to plot values
(defdf *plot-stacked* (matrix-df '(sample name height-cm weight-g-bag dia-mm-ten bin-status field-status row-type hulless-condition sblotch-rating)
            (stack-rows
             ;; first map to matching order and add NA fill for needed columns
             (map-df *plot-all*
                     '(sample name height-cm weight-g-bag dia-mm-ten bin-status field-status row-type hulless-condition sblotch-rating)
                     (lambda (s n h w d bs fs rt hc sr)
                       (vector s n h w d bs fs rt hc sr))
                     '(sample name height-cm weight-g-bag dia-mm-ten bin-status field-status row-type hulless-condition sblotch-rating))
             (map-df *plotspecial-all*
                     '(sample name height-cm weight-g-bag dia-mm-ten)
                     (lambda (s n h w d)
                       (vector s n h w d :na :na :na :na :na))
                     '(sample name height-cm weight-g-bag dia-mm-ten bin-status field-status row-type hulless-condition sblotch-rating)))))

;; ====================================== calculate adjustments

(defparameter *bag-per-bundle* (/ ;; mean per bag
                                 (/ ;; mean per bundle
                                  (+
                                   939.1 ;; 100 bag bundles weight grams
                                   934.7
                                   936.4
                                   930.9)
                                  4)
                                 100))

(defparameter *tape-per-bundle* (/ 47.4 36)) ;; tape weight per unit

(defparameter *loss-per-bundle* (/ 810.6 1040)) ; loose loss per unit after  drying and weighing

(def *adjustment-weight* ;; total adjustment to gross weight
  (+
   (- (+
       *bag-per-bundle*
       *tape-per-bundle*))
   *loss-per-bundle*))

(def *adjustment-diameter* -10) ;; 10mm designed bias at read line of measurement


;; ====================================== insert means and values

;;       SAMPLE NAME                      HEIGHT-CM    WEIGHT-G-BAG DIA-MM-TEN BIN-STATUS FIELD-STATUS ROW-TYPE   HULLESS-CONDITION SBLOTCH-R
;;  997    998 BM1731-030                     71         89.7         51         NA           NA        2                 C              3
;;  998    999 Wheat                          73         77.5         48         NA           NA        W                 W              0
;;  999   1000 Wheat                           0          nan*       nan*     not-in-bin not-in-field   W                 W              0
;; 1000   1001 guard_2                        77*        -2.0*        -2*        NA           NA       NA*               NA*            NA*
;; 1001   1002 guard_6                        76*        -2.0*        -2*        NA           NA       NA*               NA*            NA*
;; 1002   1003 weed                           10         20.0         20         NA           NA       NA*               NA*            NA*
;; 1003   1004 soil                            0         -1.0         -1         NA           NA       NA*               NA*            NA*
;; 1004   1005 GCP_low                         0         -1.0         -1         NA           NA       NA                NA             NA
;; 1005   1006 GCP_high                      100         -1.0         -1         NA           NA       NA                NA             NA
;; 1006   1007 irrigation                     10         -1.0         -1         NA           NA       NA                NA             NA
;; 1007   1008 test_black                      0          0.0          0         NA           NA       NA                NA             NA
;; 1008   1009 test_white                      1          1.0          1         NA           NA       NA                NA             NA
;; 1009   1010 null                            0          0.0          0         NA           NA       NA*               NA*            NA*

(variables)

(defparameter *g2-hulless* "C")
(defparameter *g6-hulless* "C")
(defparameter *g2-sblotch* 7)
(defparameter *g6-sblotch* 7)

(defun specific-heights (sample height-cm)
  (case sample
    (1001 *g2-mean-height*)
    (1002 *g6-mean-height*)
    (otherwise height-cm)))

(defun specific-weightgbag (sample weight-g-bag)
  (case sample
    (1001 *g2-mean-weight-raw*)
    (1002 *g6-mean-weight-raw*)
    (otherwise weight-g-bag)))

(defun specific-diammten (sample dia-mm-ten)
  (case sample
    (1001 (sig-fig 0 *g2-mean-diameter-raw*))
    (1002 (sig-fig 0 *g6-mean-diameter-raw*))
    (otherwise dia-mm-ten)))

(defun specific-rowtypes (sample row-type)
  "set row types: 2 6 -1 -1 -1"
  (case sample
    (1001 2)
    (1002 6)
    (1003 -1)
    (1004 -1)
    (1010 -1)
    (otherwise row-type)))

(defun specific-hulless (sample hulless-condition)
  "set hulless: v v -1 -1 -1"
  (case sample
    (1001 *g2-hulless*)
    (1002 *g6-hulless*)
    (1003 -1)
    (1004 -1)
    (1010 -1)
    (otherwise hulless-condition)))

(defun specific-sblotch (sample sblotch-rating)
  "set blotch ratings: v v -1 -1 -1"
  (case sample
    (1001 *g2-sblotch*)
    (1002 *g6-sblotch*)
    (1003 -1)
    (1004 -1)
    (1010 -1)
    (otherwise sblotch-rating)))

;; #(SAMPLE NAME HEIGHT-CM WEIGHT-G-BAG DIA-MM-TEN BIN-STATUS FIELD-STATUS ROW-TYPE HULLESS-CONDITION SBLOTCH-RATING)
(replace-column! *plot-stacked*
                 'height-cm
                 (map-rows *plot-stacked* '(sample height-cm)
                           #'specific-heights))
(replace-column! *plot-stacked*
                 'weight-g-bag
                 (map-rows *plot-stacked* '(sample weight-g-bag)
                           #'specific-weightgbag))
(replace-column! *plot-stacked*
                 'dia-mm-ten
                 (map-rows *plot-stacked* '(sample dia-mm-ten)
                           #'specific-diammten))
(replace-column! *plot-stacked*
                 'row-type
                 (map-rows *plot-stacked* '(sample row-type)
                           #'specific-rowtypes))
(replace-column! *plot-stacked*
                 'hulless-condition
                 (map-rows *plot-stacked* '(sample hulless-condition)
                           #'specific-hulless))
(replace-column! *plot-stacked*
                 'sblotch-rating
                 (map-rows *plot-stacked* '(sample sblotch-rating)
                           #'specific-sblotch))
(tail *plot-stacked* 20)
;; ====================================== columnar adjustments
(defun adjust-weights (weight-g-bag)
  "adjust a raw weight value, set nan t0 -1, retain :na, -1, 0"
  (cond
    ((equalp weight-g-bag "nan") -1)
    ((equal weight-g-bag :na) :na)
    ((= weight-g-bag -1) -1)
    ((= weight-g-bag 1) 1)
    ((= weight-g-bag 0) 0)
    (t (float (sig-fig 1 (+ weight-g-bag *adjustment-weight*))))))

(defun adjust-diameters (dia-mm-ten)
  "adjust a raw diameter value, set nan t0 -1, retain :na, 1, -1, 0"
  (cond
    ((equalp dia-mm-ten "nan") -1)
    ((equal dia-mm-ten :na) :na)
    ((= dia-mm-ten -1) -1)
    ((= dia-mm-ten 1) 1)
    ((= dia-mm-ten 0) 0)
    (t (sig-fig 0(+ dia-mm-ten *adjustment-diameter*)))))

(add-column! *plot-stacked*
             'weight
             (map-rows *plot-stacked* '(weight-g-bag)
                       #'adjust-weights))

(add-column! *plot-stacked*
             'diameter
             (map-rows *plot-stacked* '(dia-mm-ten)
                       #'adjust-diameters))

(remove-column! *plot-stacked* 'bin-status)
(remove-column! *plot-stacked* 'field-status)
(remove-column! *plot-stacked* 'dia-mm-ten)
(remove-column! *plot-stacked* 'weight-g-bag)

;; ====================================== columnar creation

                                        ; normalizations
(defun normalize-area (diameter)
  "area: pi * (DIAMETER/2)^2"
  (cond
    ((equal diameter :na) :na)
    ((= diameter -1) -1)
    ((= diameter 1) 1)
    ((= diameter 0) 0)
    (t (sig-fig 0 (* pi
                     (expt (/ diameter 2)
                           2))))))

(defun stem-weight (weight area)
  "weight per individual stem: weight/area"
  (cond
    ((equal area :na) :na)
    ((= area -1) -1)
    ((= area 1) 1)
    ((= area 0) 0)
    (t (float (sig-fig 4 (/ weight area))))))

(defun density-estimate (weight height area)
  "density estimate: WEIGHT / (HEIGHT-CM * AREA)"
  (cond
    ((equal area :na) :na)
    ((or (= area -1)
         (= height -1)) -1)
    ((or (= area 1)
         (= height 1)) 1)
    ((or(= area 0)
        (= height 0)) 0)
    (t (float (sig-fig 6 (/ weight (* height area)))))))

(add-column! *plot-stacked*
             'area
             (map-rows *plot-stacked* '(diameter)
                       #'normalize-area))

(add-column! *plot-stacked*
             'stem-weight
             (map-rows *plot-stacked* '(weight area)
                       #'stem-weight))

(add-column! *plot-stacked*
             'density
             (map-rows *plot-stacked* '(weight height-cm area)
                       #'density-estimate))

                                        ; categoricals
(defun categorize-rows (row-type)
  "ROW-TYPE -> 2 6 -1:not valid)"
  (case row-type
    (2 2)
    (6 6)
    (otherwise -1)))

(defun categorize-barley-wheat (row-type)
  "ROW-TYPE -> barley-wheat 1:barley 2:wheat -1:invalid"
  (cond
    ((or (equal row-type 2)
         (equal row-type 6)) 1)
    ((or (equal row-type "W")
         (equal row-type "D")) 2)
    (t -1)))

(defun categorize-hulled (hulless-condition)
  "hulless condition -> 0:hulless 1:hulled -1:invalid"
  (cond
    ((equal hulless-condition "H") 0)
    ((equal hulless-condition "C") 1)
    (t -1)))

(defun categorize-sblotch (sblotch-rating)
  "SBLOTCH-RATING: groups  0:Low (0-3), 1:Medium (4-6), 2:High (7+) -1:invalid"
  (case sblotch-rating
    (0 1)
    (1 1)
    (2 1)
    (3 1)
    ;;
    (4 2)
    (5 2)
    (6 2)
    ;;
    (7 3)
    (8 3)
    (9 3)
    (otherwise -1)))

(add-column! *plot-stacked*
             'rows
             (map-rows *plot-stacked* '(row-type)
                       #'categorize-rows))

(add-column! *plot-stacked*
             'barley-wheat
             (map-rows *plot-stacked* '(row-type)
                       #'categorize-barley-wheat))

(add-column! *plot-stacked*
             'hulled
             (map-rows *plot-stacked* '(hulless-condition)
                       #'categorize-hulled))

(remove-column! *plot-stacked* 'sblotch-lmh)
(add-column! *plot-stacked*
             'sblotch-lmh
             (map-rows *plot-stacked* '(sblotch-rating)
                       #'categorize-sblotch))

(tail *plot-stacked* 20)

;;;; =================================== end of executed code
(uiop:quit)
;; ====================================== build

;; ====================================== distributions outliers and clean
                                        ; set browser for viewing plots
(setf plot:*browser-commands* '((:CHROME) (:FIREFOX . "firefox") (:DEFAULT . "nyxt")))

                                        ; source dimensions

;; HEIGHT-CM
(plot:plot
 (vega:defplot height-cm
   `(:mark :bar
	   :data (:values ,(filter-rows *plot-stacked* '(and (> 100 height-cm)
                                                   (< 15 height-cm))))
	   :encoding (:x (:bin (:step 1)
	                  :field :height-cm)
		            :y (:aggregate :count)))))
;; observe a few infrequent high outliers, due to photography and plant proximity
;; overall normal with a preponderance of high values

;; ROW-TYPE
(length *plot-stacked*:row-type)
(remove-duplicates *plot-stacked*:row-type)

(defun unique-with-counts (vector &key (test #'eql))
  (let* ((counts-alist (reduce (lambda (acc item)
                                 (let ((found (assoc item acc :test test)))
                                   (if found
                                       (progn (incf (cdr found)) acc)
                                       (cons (cons item 1) acc))))
                               (coerce vector 'list)
                               :initial-value nil))
         (unique-items (make-array (length counts-alist)))
         (counts (make-array (length counts-alist))))
    (loop for entry in counts-alist
          for i from 0
          do (setf (aref unique-items i) (car entry)
                   (aref counts i) (cdr entry)))
    (values unique-items counts)))

(unique-with-counts *plot-stacked*:row-type)

;; => #(:NA -1 2 6 "W" "D"), #(5 3 897 58 37 10)

;; HULLESS-CONDITION
(unique-with-counts *plot-stacked*:hulless-condition :test 'equal)
;; => #(:NA -1 "H" "C" "W" "D"), #(5 3 91 864 37 10)

;; SBLOTCH-RATING
(unique-with-counts *plot-stacked*:sblotch-rating :test 'equal)
;; => #(:NA -1 9 5 4 7 2 6 3 8 1 0), #(5 23 65 117 169 109 90 121 162 94 9 46)
(plot:plot
 (vega:defplot simple-bar-chart
   `(:mark :bar
     :data (:values ,(plist-df '(:a #(:NA -1 9 5 4 7 2 6 3 8 1 0)
	                               :b #(5 23 65 117 169 109 90 121 162 94 9 46))))
     :encoding (:x (:field :a :type :nominal :axis ("labelAngle" 0))
                :y (:field :b :type :quantitative)))))
;; preponderance of 3 and 4 scores


;; DIAMETER
(plot:plot
 (vega:defplot diameter
   `(:mark :bar
	   :data (:values ,(filter-rows  *plot-stacked* '(< 1 diameter)))
	   :encoding (:x (:bin (:step 2)
	                  :field :diameter)
		            :y (:aggregate :count)))))
;;normal no outliers

;; WEIGHT

(plot:plot
 (vega:defplot weight
   `(:mark :bar
	   :data (:values ,(filter-rows  *plot-stacked* '(< 1 weight)))
	   :encoding (:x (:bin (:step 2)
	                  :field :weight)
		            :y (:aggregate :count)))))
;; normal with a few high outliers

;; AREA
(plot:plot
 (vega:defplot area
   `(:mark :bar
     :width 300
	   :data (:values ,(filter-rows  *plot-stacked* '(< 1 area)))
	   :encoding (:x (:bin (:step 50)
	                  :field :area)
		            :y (:aggregate :count)))))
;; normal no outliers

                                        ; iterated with the categorization function to balance

;; SBLOTCH-LMH
(unique-with-counts (column *plot-stacked* 'sblotch-lmh) :test 'equal)
;; => #(-1 2 3 1), #(28 407 268 307)

(plot:plot
 (vega:defplot simple-bar-chart
   `(:mark :bar
     :data (:values ,(plist-df '(:a #(-1 2 3 1)
	                               :b #(28 407 268 307))))
     :encoding (:x (:field :a :type :nominal :axis ("labelAngle" 0))
                :y (:field :b :type :quantitative)))))
;; moderately balanced data set between l m h

;; ====================================== write file

(write-csv *plot-stacked*
           (merge-pathnames "field-data.csv" *data-path*)
           :add-first-row t)         ; add column headers
