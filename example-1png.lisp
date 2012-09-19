(ql:quickload :cffi)
(load "cl-libplot.lisp")

(defpackage "EXAMPLE-1-PNG"
  (:use "COMMON-LISP" "CFFI" "CL-LIBPLOT"))

(in-package "EXAMPLE-1-PNG")
(defparameter *my-file-pointer* (foreign-funcall "fopen" :string "example-1.png" :string "wb" :pointer))
(defparameter *plotter-params* (newplparams))
(setplparam *plotter-params* "BITMAPSIZE" "800x800+0+0")
(defparameter *plotter* (newpl "png"
			       (foreign-symbol-pointer "stdin")
			       *my-file-pointer*
			       (foreign-symbol-pointer "stderr")
			       *plotter-params*))
(openpl *plotter*)
(lp-fspace *plotter* 0d0 0d0 1000d0 1000d0)
(flinewidth *plotter* 0.25d0)
(pencolorname *plotter* "red")
(fmove *plotter* 600d0 300d0)

(defun draw-c-curve (plotter x0 y0 &key (maxorder 12))
  (labels ((rec (dx dy order)
	     (cond ((>= order maxorder)
		    (fcontrel plotter dx dy))
		   (t
		    (rec (* 0.5d0 (- dx dy)) (* 0.5d0 (+ dx dy)) (1+ order))
		    (rec (* 0.5d0 (+ dx dy)) (* 0.5d0 (- dy dx)) (1+ order))))))
    (rec x0 y0 0)))

(draw-c-curve *plotter* 0d0 400d0)
(closepl *plotter*)
(deletepl *plotter*)
(deleteplparams *plotter-params*)
(foreign-funcall "fclose" :pointer *my-file-pointer* :int)
