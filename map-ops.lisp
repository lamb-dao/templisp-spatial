;;;; ===================================  set environment
                                        ; imports
(ql:quickload :click)

(defpackage :map-ops
  (:use
   :cl
   :click
   :cmd
   :py4cl2
   :file-finder)
  (:shadowing-import-from :cmd :current-directory))

                                        ; enter package
(in-package :map-ops) ; Also enter this in the REPL!


;;;; =================================== merge east-west processing to single standard flight

(defparameter *east-dir* #P"/bulk-1/2023/Pix4D/X2023-07-24_UNMERGED/2023-07-24_eastside/4_index/indices/")
(defparameter *west-dir* #P"/bulk-1/2023/Pix4D/X2023-07-24_UNMERGED/2023-07-24_re-run_westside/4_index/indices/")
(defparameter *merge-dir* #P"/bulk-1/2023/Pix4D/X2023-07-24_UNMERGED/2023-07-24_merges/")

(defun F-to-P (F)
  "Convert one #F filename to #P pathname"
  (probe-file (path F)))

(defun east-tifs ()
  "returns a list of #P, being the .tif files below *east-dir*"
  (mapcar #'F-to-P (finder* :root *east-dir*
                            :predicates (list
                                         (extension= "tif")
                                         (complement (path~ "tiles"))))))

(defun west-tifs ()
  "returns a list of #P, being the .tif files below *west-dir*"
  (mapcar #'F-TO-P (finder* :root *west-dir*
                            :predicates (list
                                         (extension= "tif")
                                         (complement (path~ "tiles"))))))

(defun assemble-out-tif (east-tif west-tif)
  "use east-tif filename to create file path located at *merge-dir*"
  (merge-pathnames (file-namestring east-tif) *merge-dir*))

(defun out-tifs ()
  (mapcar #'assemble-out-tif (east-tifs) (west-tifs)))

(defun check-tif-sets (o w e)
  (flet ((index-only (p) (second (str:split "index_" (pathname-name p)))))
    ;; all same length
    (assert (= (length o)
               (length w)
               (length e)))
    ;; all are type #P
    (assert (every #'pathnamep (append o w e)))
    ;; all have same indices
    ;;&&& not working
    (assert (and (null (set-difference (mapcar #'index-only o)
                                       (mapcar #'index-only w) :test #'string=))
                 (null (set-difference (mapcar #'index-only o)
                                       (mapcar #'index-only e) :test #'string=))))
    ;; all have all indices, by checking 1, as all are K same
    (assert (null (set-difference '("sentera_ndre" "blue" "green" "ndvi" "nir" "red" "red_edge")
                                  (mapcar #'index-only e) :test #'string=)))
    ;; each file of west and east actually exists
    (assert (every #'probe-file (append e w)))))

(defun gdal_merge.py (out-tif bottom-tif top-tif)
  "Usage: gdal_merge.py [-o out_filename] [-of out_format] [-co NAME=VALUE]*
                     [-ps pixelsize_x pixelsize_y] [-tap] [-separate] [-q] [-v] [-pct]
                     [-ul_lr ulx uly lrx lry] [-init \"value [value...]\"]
                     [-n nodata_value] [-a_nodata output_nodata_value]
                     [-ot datatype] [-createonly] input_files
                     [--help-general] "
  (cmd:cmd (format nil "gdal_merge.py -o ~A ~A ~A" (path out-tif) (path bottom-tif) (path top-tif))))

(defun merge-all-indices ()
  (let ((o (out-tifs))
        (w (west-tifs))
        (e (east-tifs)))
    (check-tif-sets o w e)
    (mapcar #'gdal_merge.py o w e)))

 ;; (merge-all-indices)

;; ====================================== rename

(defun rename-tifs (&key path replace with (do nil))
  "Find tifs and fix names"
  (let* ((pix-root  "/bulk-1/2023/Pix4D/")
         (files (mapcar #'F-to-P (finder* :root pix-root
                         :predicates (list (extension= "tif")
                                           (path~ "4_index")
                                           (path~ "indices")
                                           (path~ path)
                                           (complement (path~ "X2023"))
                                           (complement (path~ "tiles"))))))
         ;; get pathname-name
         (names (mapcar #'pathname-name
                        files))
         ;; make new name
         (new-names (mapcar #'(lambda (n)
                                (str:replace-first replace with n))
                            names)))
    (assert (= 0 (mod (length files) 7)))
    (format t "n files to rename: ~A" (length files))
    (print files)
    (print names)
    (print new-names)

    ;;rename, controlled by :do t
    (when do (mapcar #'(lambda (f n)
                (rename-file f (make-pathname :name n)))
            files new-names))))

;; normalize naming
(rename-tifs :path  "10MFlight_SMS_Pix4d_DroboTest"
             :replace "10MFlight_SMS_Pix4d_DroboTest"
             :with "SHFlight_SenteraMS_9M")

;; normalize naming
(rename-tifs :path "teraMS_9M_01_ind"
             :replace "_01_"
             :with "_")
;; fix L typo
(rename-tifs :path  "SHFLight_SenteraMS_9M"
             :replace "SHFL"
             :with "SHFl")

;; all 70 index files to short form
(rename-tifs :path "SHFlight_SenteraMS_9M_index"
             :replace "_SHFlight_SenteraMS_9M_"
             :with "_")

;; ====================================== copy all tifs

;; gather a copy of all tifs to a single dir
(defun collect ()
  (mapcar #'(lambda (f)
              (uiop:copy-file f (make-pathname :defaults f
                                               :directory (pathname-directory #P"/bulk-1/rasters/"))))
          (mapcar #'F-to-P (finder* :root "/bulk-1/2023/Pix4D/"
                                    :predicates (list (extension= "tif")
                                                      (path~ "4_index")
                                                      (path~ "indices")
                                                      (complement (path~ "X2023"))
                                                      (complement (path~ "tiles")))))))
;; (collect)

;; ====================================== clip to AOI buffered

(defun gdalwarp-clip (gpkg-shape srs-tif dst-tif)
  "clip srs-tif to the shape defined in gpkg-shape, saving as dst-tif"
  ($cmd (format nil "gdalwarp -of GTiff -cutline ~A -crop_to_cutline ~A ~A" gpkg-shape srs-tif dst-tif)))

(defun clip-to-buffer (srs-tif)
  "Apply AOI buffered shape, to srs-tif and make a new name appending _buffer"
  (let* ((shape #P"/home/user/qgis/AOI-buffered.gpkg")
         (postfix "_buffer")
         (new-name (make-pathname :defaults srs-tif
                                  :name (concatenate 'string
                                                     (pathname-name srs-tif)
                                                     postfix))))
    (gdalwarp-clip shape srs-tif new-name)))

(defun tifs-clipped-to-buffer ()
  "Gathers all full size .tif and clips to buffered aoi, tests for 70"
  (let ((tifs (mapcar #'F-to-P (finder* :root "/bulk-1/rasters/"
                                         :predicates (list (extension= "tif")
                                                           (complement (name~ "_buffer")))))))
    (assert (= 70 (length tifs)) () "Expected 70 found ~A" (length tifs))
    (mapcar #'clip-to-buffer tifs)))

;;(tifs-clipped-to-buffer)


;; ====================================== clip to test buffered,  repeat clip for test area!
(defun clip-to-test-buffer (srs-tif)
  "Apply test buffered shape, to srs-tif and make a new name appending _test-buffer"
  (let* ((shape #P"/home/user/qgis/test-AOI-buffered.gpkg")
         (postfix "_test-buffer")
         (new-name (make-pathname :defaults srs-tif
                                  :name (concatenate 'string
                                                     (pathname-name srs-tif)
                                                     postfix))))
    (gdalwarp-clip shape srs-tif new-name)))

(defun tifs-clipped-to-test-buffer ()
  "Gathers all full size .tif and clips to buffered test area, tests for 70"
  (let ((tifs (mapcar #'F-to-P (finder* :root "/bulk-1/rasters/"
                                         :predicates (list (extension= "tif")
                                                           (complement (name~ "_test-buffer"))
                                                           (complement (name~ "_buffer")))))))
    (assert (= 70 (length tifs)) () "Expected 70 found ~A" (length tifs))
    (mapcar #'clip-to-test-buffer tifs)))

;;(tifs-clipped-to-test-buffer)

;; ====================================== fill pixels with no data

(defun gdal_fillnodata (srs-tif dst-tif)
  "heal nodata pixels in srs-tif, saving as dst-tif"
  ($cmd (format nil "gdal_fillnodata.py -md 100 -si 11 -of GTiff ~A ~A" srs-tif dst-tif)))

(defun fillnodata (srs-tif)
  "fill pixels in srs-tif, create a new name, appending _filled"
  (let* ((postfix "_filled")
         (new-name (make-pathname :defaults srs-tif
                                  ::name (concatenate 'string
                                                      (pathname-name srs-tif)
                                                      postfix))))
    (gdal_fillnodata srs-tif new-name)))

(defun tifs-filled ()
  "gather all clipped tiffs and fills nodata pixels, tests for 70+70 "
  (let* ((test-tifs (mapcar #'F-to-P (finder* :root "/bulk-1/rasters/"
                                              :predicates (list (extension= "tif")
                                                                (name~ "_test-buffer")
                                                                (complement (name~ "_filled"))))))
         (full-tifs (mapcar #'F-to-P (finder* :root "/bulk-1/rasters/"
                                              :predicates (list (extension= "tif")
                                                                (name~ "_buffer")
                                                                (complement (name~ "_filled"))))))
         (tifs (append test-tifs full-tifs)))
    (assert (= 140 (length tifs)) () "Expected 140, found ~A" (length tifs))
    (mapcar #'fillnodata tifs)))

;;(tifs-filled)

;; ====================================== pixel alignment and resampling
(defun gdalwarp-tap (src-tif dst-tif)
  "resample to 3mm pixels and target aligned pixels"
  ($cmd (format nil "gdalwarp -tr 0.003 0.003 -r average -tap -of GTiff -overwrite ~A ~A" src-tif dst-tif)))

(defun resample-tap (srs-tif)
  "align pixels in srs-tif, create a new name, appending _aligned"
  (let* ((postfix "_aligned")
         (new-name (make-pathname :defaults srs-tif
                                  ::name (concatenate 'string
                                                      (pathname-name srs-tif)
                                                      postfix))))
    (gdalwarp-tap srs-tif new-name)))

(defun tifs-aligned ()
  "gather all filled tiffs and aligns pixels, tests for 70+70 "
  (let* ((test-tifs (mapcar #'F-to-P (finder* :root "/bulk-1/rasters/"
                                              :predicates (list (extension= "tif")
                                                                (name~ "_test-buffer_filled")
                                                                (complement (name~ "_aligned"))))))
         (full-tifs (mapcar #'F-to-P (finder* :root "/bulk-1/rasters/"
                                              :predicates (list (extension= "tif")
                                                                (name~ "_buffer_filled")
                                                                (complement (name~ "_aligned"))))))
         (tifs (append test-tifs full-tifs)))
    (assert (= 140 (length tifs)) () "Expected 140, found ~A" (length tifs))
    (mapcar #'resample-tap tifs)
    ))

;;(tifs-aligned)

;; ====================================== normalize

(pyversion-info)    ; fails if python command is not resolved in system
(defpymodule "sys" nil :lisp-package "SYS")
(defpymodule "pprint" nil :lisp-package "PPRINT")
(pyexec "sys.path.append('/home/user/db/1/masters/code/templisp-spatial')")
(pyexec "sys.path.append('/home/user/.local/lib/python3.10/site-packages')")
(pyexec "sys.path.append('/home/user/.guix-profile/lib/python3.10/site-packages')")
;; check path contains target and path to imported modules
(pyexec "pprint.pprint(sys.path)")
;; check file is on path
(defpymodule "os" nil :lisp-package "OS")
(os:getcwd)
(os:listdir)

(defpymodule "dynamicWorldNorm" nil :lisp-package "DWN")

(defun normalize (srs-tif)
  "norm srs-tif, create a new name, appending _normed, identify if index or not"
  (let* ((postfix "_normed")
         (new-name (make-pathname :defaults srs-tif
                                  :name (concatenate 'string
                                                     (pathname-name srs-tif)
                                                     postfix)))
         ;; set indexp t if srs-tif name contains 'ndvi' or 'ndre'
         (indexp (if (or (search "_ndvi" (namestring srs-tif))
                         (search "_ndre" (namestring srs-tif)))
                     t
                     nil)))

    (dwn:run :src-file (namestring srs-tif)
             :dst-file (namestring new-name)
             :plot t
             :indexp indexp)))

(defun tifs-normalized ()
  "gather all aligned tiffs and normalize, tests for 70+70 "
  (let* ((test-tifs (mapcar #'F-to-P (finder* :root "/bulk-1/rasters/"
                                              :predicates (list (extension= "tif")
                                                                (name~ "_test-buffer")
                                                                (name~ "_aligned")
                                                                (complement (name~ "_normed"))))))
         (full-tifs (mapcar #'F-to-P (finder* :root "/bulk-1/rasters/"
                                              :predicates (list (extension= "tif")
                                                                (name~ "_buffer")
                                                                (name~ "_aligned")
                                                                (complement (name~ "_normed"))))))
         (tifs (append test-tifs full-tifs)))
    (assert (= 140 (length tifs)) () "Expected 140, found ~A" (length tifs))
    (mapcar #'normalize tifs)))

;;(tifs-normalized)

(py4cl2:pystop)
(py4cl2:python-alive-p)

;; ====================================== fill pixels with no data

(defun gdal_fillnodata (srs-tif dst-tif)
  "heal nodata pixels in srs-tif, saving as dst-tif"
  ($cmd (format nil "gdal_fillnodata.py -md 100 -si 11 -of GTiff ~A ~A" srs-tif dst-tif)))

(defun fillnodata (srs-tif)
  "fill pixels in srs-tif, create a new name, appending _filled"
  (let* ((postfix "_filled")
         (new-name (make-pathname :defaults srs-tif
                                  ::name (concatenate 'string
                                                      (pathname-name srs-tif)
                                                      postfix))))
    (format t "Fillnodata: ~A" srs-tif)
    (gdal_fillnodata srs-tif new-name)))

(defun tifs-filled ()
  "gather all clipped tiffs and fills nodata pixels, tests for 70+70 "
  (let* ((test-tifs (mapcar #'F-to-P (finder* :root "/bulk-1/rasters/"
                                              :predicates (list (extension= "tif")
                                                                (name~ "_test-buffer")
                                                                (name~ "_normed")
                                                                (complement (name~ "_filled.tif"))))))
         (full-tifs (mapcar #'F-to-P (finder* :root "/bulk-1/rasters/"
                                              :predicates (list (extension= "tif")
                                                                (name~ "_buffer")
                                                                (name~ "_normed")
                                                                (complement (name~ "_filled.tif"))))))
         (tifs (append test-tifs full-tifs)))
    (assert (= 140 (length tifs)) () "Expected 140, found ~A" (length tifs))
    (mapcar #'fillnodata tifs)))

;;(tifs-filled)

;;#|;; vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv end of compiled code
(error "Beyond here be monsters") ;; ensure #| is active to exclude construction from compilation

;; ======================================build
;; gaussian pyramid

;; ====================================== scratch


;; ====================================== reference

;;;; vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv later

;; clip to AOI
;; export to gpkg
