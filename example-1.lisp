(ql:quickload :cffi)
(load "cl-libplot.lisp")

(defparameter *plotter-params* (cllp:newplparams))
(cllp:setplparam *plotter-params* "BITMAPSIZE" "800x800+0+0")
(defparameter *plotter* (cllp:newpl "X"
				    (cffi:foreign-symbol-pointer "stdin")
				    (cffi:foreign-symbol-pointer "stdout")
				    (cffi:foreign-symbol-pointer "stderr")
				    *plotter-params*))
(cllp:openpl *plotter*)
(cllp:lp-fspace *plotter* 0d0 0d0 1000d0 1000d0)
(cllp:flinewidth *plotter* 0.25d0)
(cllp:pencolorname *plotter* "red")
(cllp:fmove *plotter* 600d0 300d0)

(defun draw-c-curve (plotter x0 y0 &key (maxorder 12))
  (labels ((rec (dx dy order)
	     (cond ((>= order maxorder)
		    (cllp:fcontrel plotter dx dy))
		   (t
		    (rec (* 0.5d0 (- dx dy)) (* 0.5d0 (+ dx dy)) (1+ order))
		    (rec (* 0.5d0 (+ dx dy)) (* 0.5d0 (- dy dx)) (1+ order))))))
    (rec x0 y0 0)))

(draw-c-curve *plotter* 0d0 400d0)
(cllp:closepl *plotter*)
(cllp:deletepl *plotter*)
