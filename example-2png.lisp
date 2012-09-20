(defpackage "EXAMPLE-2-PNG"
  (:use "COMMON-LISP" "CFFI" "CL-LIBPLOT"))

(in-package "EXAMPLE-2-PNG")
(defparameter *SIZE* 100.0d0)
(defparameter *EXPAND* 2.2d0)
(defparameter *my-file-pointer* (foreign-funcall "fopen" :string "example-2.png" :string "wb" :pointer))
(defparameter *plotter-params* (newplparams))
(setplparam *plotter-params* "PAGESIZE" "a4")
(defparameter *plotter* (newpl "png"
			       (foreign-symbol-pointer "stdin")
			       *my-file-pointer*
			       (foreign-symbol-pointer "stderr")
			       *plotter-params*))
(openpl *plotter*)
(lp-fspace *plotter* (- *SIZE*) (- *SIZE*) *SIZE* *SIZE*)
(pencolorname *plotter* "blue")
(fillcolorname *plotter* "white")
(filltype *plotter* 1)
;;(fontname *plotter* "NewCenturySchlbk-Roman")

(defun draw-box-string (plotter s size angle)
  (ftextangle plotter angle)
  (let ((true-size (ffontsize plotter size))
	(width (flabelwidth plotter s)))
    (fellipserel plotter 0d0 0d0 (* *EXPAND* width 0.5d0) (* *EXPAND* true-size 0.5d0) angle)
    (alabel plotter 99 99 s)))

(do* ((i 80 (1- i))
      (theta (* 0.5d0 i) (* 0.5d0 i))
      (radius (/ *SIZE* (expt theta 0.35d0)) (/ *SIZE* (expt theta 0.35d0))))
     ((= i 1))
  (fmove *plotter* (* radius (cos theta)) (* radius (sin theta)))
  (draw-box-string *plotter* "GNU libplot!" (* 0.04d0 radius) (- (/ (* 180d0 theta) pi) 90d0)))


(closepl *plotter*)
(deletepl *plotter*)
(deleteplparams *plotter-params*)
(foreign-funcall "fclose" :pointer *my-file-pointer* :int)
