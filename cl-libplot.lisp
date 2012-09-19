(defpackage :cl-libplot
  (:use :common-lisp :cffi)
  (:nicknames "LIBPLOT" "CLLP")
  (:documentation "Common Lisp binding to GNU's libplot, part of the plotutils package.")
  (:export "NEWPL" "DELETEPL" "NEWPLPARAMS" "DELETEPLPARAMS" "COPYPLPARAMS" "SETPLPARAM"
	   "ARC" "BOX" "CIRCLE" "CLOSEPL" "CONT" "ERASE" "LABEL" "LINE" "LINEMOD" "OPENPL"
	   "POINT" "MOVE" "LP-SPACE" "ALABEL" "ARCREL" "BEZIER2" "BEZIER2REL" "BEZIER3"
	   "BEZIER3REL" "BGCOLOR" "BGCOLORNAME" "BOXREL" "CAPMOD" "CIRCLEREL" "CLOSEPATH"
	   "COLOR" "COLORNAME" "CONTREL" "ELLARC" "ELLARCREL" "ELLIPSE" "ELLIPSEREL"
	   "ENDPATH" "ENDSUBPATH" "FILLCOLOR" "FILLCOLORNAME" "FILLMODE" "FILLTYPE"
	   "FLUSHPL" "FONTNAME" "FONTSIZE" "HAVECAP" "JOINMOD" "LABELWIDTH" "LINEDASH"
	   "LINEREL" "LINEWIDTH" "MARKER" "MARKERREL" "MOVEREL" "ORIENTATION" "PENCOLOR"
	   "PENCOLORNAME" "PENTYPE" "POINTREL" "RESTORESTATE" "SAVESTATE" "LP-SPACE2"
	   "TEXTANGLE" "FFONTNAME" "FFONTSIZE" "FLABELWIDTH" "FTEXTANGLE" "FARC"
	   "FARCREL" "FBEZIER2" "FBEZIER2REL" "FBEZIER3" "FBEZIER3REL" "FBOX" "FBOXREL"
	   "FCIRCLE" "FCIRCLEREL" "FCONT" "FCONTREL" "FELLARC" "FELLARCREL"
	   "FELLIPSE" "FELLIPSEREL" "FLINEDASH" "FLINE" "FLINEREL" "FLINEWIDTH"
	   "FMARKER" "FMARKERREL" "FMOVE" "FMOVEREL" "FPOINT" "FPOINTREL" 
	   "LP-FSPACE" "LP-FSPACE2" "FCONCAT" "FMITERLIMIT" "FROTATE" "FSCALE" "SETMATRIX"
	   "FTRANSLATE"))
   
(in-package :cl-libplot)

(define-foreign-library libplot
  (:unix (:or "libplot.so.2" "libplot.so"))
  (t (:default "libplot")))

(use-foreign-library libplot)

(defcfun ("pl_newpl_r" newpl) :pointer
  "Constructor for the plPlotter type.  Parameter values are
specified at creation time via a plPlotterParams instance.
Bound to pl_newpl-r."
  (type :string)
  (infile :pointer)
  (outfile :pointer)
  (errfile :pointer)
  (plotter_params :pointer))

(defcfun ("pl_deletepl_r" deletepl) :int
  "Destructor for the plPlotter type.  Parameter values are
specified at creation time via a plPlotterParams instance.
Bound to pl_deletepl_r."
  (plotter :pointer))

(defcfun ("pl_newplparams" newplparams) :pointer
  "Constructor for the plPlotterParams type,
any instance of which stores parameters that are used when creating a
plPlotter. Bound to pl_newplparams.")

(defcfun ("pl_deleteplparams" deleteplparams) :int
  "Destructor for the plPlotterParams type,
any instance of which stores parameters that are used when creating a
plPlotter. Bound to pl_deleteplparams."
  (plotter-params :pointer))

(defcfun ("pl_copyplparams" copyplparams) :pointer
  "Copy constructor for the plPlotterParams type,
any instance of which stores parameters that are used when creating a
plPlotter. Bound to pl_copyplparams."
  (plotter-params :pointer))

(defcfun ("pl_setplparam" setplparam) :int
  "A function for setting a single Plotter parameter in a plPlotterParams
instance. Bound to pl_setplparam."
  (plotter-params :pointer)
  (parameter :string)
  (value :string))

(defcfun ("pl_arc_r" arc) :int
  "Bound to pl_arc_r."
  (plotter :pointer)
  (xc :int)
  (yc :int)
  (x0 :int)
  (y0 :int)
  (x1 :int)
  (y1 :int))

(defcfun ("pl_box_r" box) :int
  "Bound to pl_box_r."
  (plotter :pointer)
  (x0 :int)
  (y0 :int)
  (x1 :int)
  (y1 :int))

(defcfun ("pl_circle_r" circle) :int
  "Bound to pl_circle_r."
  (plotter :pointer)
  (x :int)
  (y :int)
  (r :int))

(defcfun ("pl_closepl_r" closepl) :int
  "Bound to pl_closepl_r."
  (plotter :pointer))

(defcfun ("pl_cont_r" cont) :int
  "Bound to pl_cont_r."
  (plotter :pointer)
  (x :int)
  (y :int))

(defcfun ("pl_erase_r" erase) :int
  "Bound to pl_erase_r."
  (plotter :pointer))

(defcfun ("pl_label_r" label) :int
  "Bound to pl_label_r."
  (plotter :pointer)
  (s :string))

(defcfun ("pl_line_r" line) :int
  "Bound to pl_line_r."
  (plotter :pointer)
  (x0 :int)
  (y0 :int)
  (x1 :int)
  (y1 :int))

(defcfun ("pl_linemod_r" linemod) :int
  "Bound to pl_linemod_r."
  (plotter :pointer)
  (s :string))

(defcfun ("pl_move_r" move) :int
  "Bound to pl_move_r."
  (plotter :pointer)
  (x :int)
  (y :int))

(defcfun ("pl_openpl_r" openpl) :int
  "Bound to pl_openpl_r."
  (plotter :pointer))

(defcfun ("pl_point_r" point) :int
  "Bound to pl_point_r."
  (plotter :pointer)
  (x :int)
  (y :int))

(defcfun ("pl_space_r" lp-space) :int
  "Bound to pl_space_r."
  (plotter :pointer)
  (x0 :int)
  (y0 :int)
  (x1 :int)
  (y1 :int))


(defcfun ("pl_alabel_r" alabel) :int
  "Bound to pl_alabel_r."
  (plotter :pointer)
  (x-justify :int)
  (y-justify :int)
  (s :string))

(defcfun ("pl_arcrel_r" arcrel) :int
  "Bound to pl_arcrel_r."
  (plotter :pointer)
  (dxc :int)
  (dyc :int)
  (dx0 :int)
  (dy0 :int)
  (dx1 :int)
  (dy1 :int))

(defcfun ("pl_bezier2_r" bezier2) :int
  "Bound to pl_bezier2_r."
  (plotter :pointer)
  (x0 :int)
  (y0 :int)
  (x1 :int)
  (y1 :int)
  (x2 :int)
  (y2 :int))

(defcfun ("pl_bezier2rel_r" bezier2rel) :int
  "Bound to pl_bezier2rel_r."
  (plotter :pointer)
  (dx0 :int)
  (dy0 :int)
  (dx1 :int)
  (dy1 :int)
  (dx2 :int)
  (dy2 :int))

(defcfun ("pl_bezier3_r" bezier3) :int
  "Bound to pl_bezier3_r."
  (plotter :pointer)
  (x0 :int)
  (y0 :int)
  (x1 :int)
  (y1 :int)
  (x2 :int)
  (y2 :int)
  (x3 :int)
  (y3 :int))

(defcfun ("pl_bezier3rel_r" bezier3rel) :int
  "Bound to pl_bezier3rel_r."
  (plotter :pointer)
  (dx0 :int)
  (dy0 :int)
  (dx1 :int)
  (dy1 :int)
  (dx2 :int)
  (dy2 :int)
  (dx3 :int)
  (dy3 :int))

(defcfun ("pl_bgcolor_r" bgcolor) :int
  "Bound to pl_bgcolor_r."
  (plotter :pointer)
  (red :int)
  (green :int)
  (blue :int))

(defcfun ("pl_bgcolorname_r" bgcolorname) :int
  "Bound to pl_bgcolorname_r."
  (plotter :pointer)
  (name :string))

(defcfun ("pl_boxrel_r" boxrel) :int
  "Bound to pl_boxrel_r."
  (plotter :pointer)
  (dx0 :int)
  (dy0 :int)
  (dx1 :int)
  (dy1 :int))

(defcfun ("pl_capmod_r" capmod) :int
  "Bound to pl_capmod_r."
  (plotter :pointer)
  (s :string))

(defcfun ("pl_circlerel_r" circlerel) :int
  "Bound to pl_circlerel_r."
  (plotter :pointer)
  (dx :int)
  (dy :int)
  (r :int))

(defcfun ("pl_closepath_r" closepath) :int
  "Bound to pl_closepath_r."
  (plotter :pointer))

(defcfun ("pl_color_r" color) :int
  "Bound to pl_color_r."
  (plotter :pointer)
  (red :int)
  (green :int)
  (blue :int))

(defcfun ("pl_colorname_r" colorname) :int
  "Bound to pl_colorname_r."
  (plotter :pointer)
  (name :string))

(defcfun ("pl_contrel_r" contrel) :int
  "Bound to pl_contrel_r."
  (plotter :pointer)
  (x :int)
  (y :int))

(defcfun ("pl_ellarc_r" ellarc) :int
  "Bound to pl_ellarc_r."
  (plotter :pointer)
  (xc :int)
  (yc :int)
  (x0 :int)
  (y0 :int)
  (x1 :int)
  (y1 :int))

(defcfun ("pl_ellarcrel_r" ellarcrel) :int
  "Bound to pl_ellarcrel_r."
  (plotter :pointer)
  (dxc :int)
  (dyc :int)
  (dx0 :int)
  (dy0 :int)
  (dx1 :int)
  (dy1 :int))

(defcfun ("pl_ellipse_r" ellipse) :int
  "Bound to pl_ellipse_r."
  (plotter :pointer)
  (x :int)
  (y :int)
  (rx :int)
  (ry :int)
  (angle :int))

(defcfun ("pl_ellipserel_r" ellipserel) :int
  "Bound to pl_ellipserel_r."
  (plotter :pointer)
  (dx :int)
  (dy :int)
  (rx :int)
  (ry :int)
  (angle :int))

(defcfun ("pl_endpath_r" endpath) :int
  "Bound to pl_endpath_r."
  (plotter :pointer))

(defcfun ("pl_endsubpath_r" endsubpath) :int
  "Bound to pl_endsubpath_r."
  (plotter :pointer))

(defcfun ("pl_fillcolor_r" fillcolor) :int
  "Bound to pl_fillcolor_r."
  (plotter :pointer)
  (red :int)
  (green :int)
  (blue :int))

(defcfun ("pl_fillcolorname_r" fillcolorname) :int
  "Bound to pl_fillcolorname_r."
  (plotter :pointer)
  (name :string))

(defcfun ("pl_fillmod_r" fillmod) :int
  "Bound to pl_fillmod_r."
  (plotter :pointer)
  (s :string))

(defcfun ("pl_filltype_r" filltype) :int
  "Bound to pl_filltype_r."
  (plotter :pointer)
  (level :int))

(defcfun ("pl_flushpl_r" flushpl) :int
  "Bound to pl_flushpl_r."
  (plotter :pointer))

(defcfun ("pl_fontname_r" fontname) :int
  "Bound to pl_fontname_r."
  (plotter :pointer)
  (s :string))

(defcfun ("pl_fontsize_r" fontsize) :int
  "Bound to pl_fontsize_r."
  (plotter :pointer)
  (size :int))

(defcfun ("pl_havecap_r" havecap) :int
  "Bound to pl_havecap_r."
  (plotter :pointer)
  (s :string))

(defcfun ("pl_joinmod_r" joinmod) :int
  "Bound to pl_joinmod_r."
  (plotter :pointer)
  (s :string))

(defcfun ("pl_labelwidth_r" labelwidth) :int
  "Bound to pl_labelwidth_r."
  (plotter :pointer)
  (s :string))

(defcfun ("pl_linedash_r" linedash) :int
  "Bound to pl_linedash_r."
  (plotter :pointer)
  (n :int)
  (dashes :pointer)
  (offset :int))

(defcfun ("pl_linerel_r" linerel) :int
  "Bound to pl_linerel_r."
  (plotter :pointer)
  (dxc :int)
  (dyc :int)
  (dx0 :int)
  (dy0 :int)
  (dx1 :int)
  (dy1 :int))

(defcfun ("pl_linewidth_r" linewidth) :int
  "Bound to pl_linewidth_r."
  (plotter :pointer)
  (size :int))

(defcfun ("pl_marker_r" marker) :int
  "Bound to pl_marker_r."
  (plotter :pointer)
  (x :int)
  (y :int)
  (type :int)
  (size :int))

(defcfun ("pl_markerrel_r" markerrel) :int
  "Bound to pl_markerrel_r."
  (plotter :pointer)
  (dx :int)
  (dy :int)
  (type :int)
  (size :int))

(defcfun ("pl_moverel_r" moverel) :int
  "Bound to pl_moverel_r."
  (plotter :pointer)
  (x :int)
  (y :int))

(defcfun ("pl_orientation_r" orientation) :int
  "Bound to pl_orientation_r."
  (plotter :pointer)
  (direction :int))

(defcfun ("pl_pencolor_r" pencolor) :int
  "Bound to pl_pencolor_r."
  (plotter :pointer)
  (red :int)
  (green :int)
  (blue :int))

(defcfun ("pl_pencolorname_r" pencolorname) :int
  "Bound to pl_pencolorname_r."
  (plotter :pointer)
  (name :string))

(defcfun ("pl_pentype_r" pentype) :int
  "Bound to pl_pentype_r."
  (plotter :pointer)
  (level :int))

(defcfun ("pl_pointrel_r" pointrel) :int
  "Bound to pl_pointrel_r."
  (plotter :pointer)
  (dx :int)
  (dy :int))

(defcfun ("pl_restorestate_r" restorestate) :int
  "Bound to pl_restorestate_r."
  (plotter :pointer))

(defcfun ("pl_savestate_r" savestate) :int
  "Bound to pl_savestate_r."
  (plotter :pointer))

(defcfun ("pl_space2_r" lp-space2) :int
  "Bound to pl_space2_r."
  (plotter :pointer)
  (x0 :int)
  (y0 :int)
  (x1 :int)
  (y1 :int)
  (x2 :int)
  (y2 :int))

(defcfun ("pl_textangle_r" textangle) :int
  "Bound to pl_textangle_r."
  (plotter :pointer)
  (angle :int))

(defcfun ("pl_ffontname_r" ffontname) :double
  "Bound to pl_ffontname_r."
  (plotter :pointer)
  (s :string))

(defcfun ("pl_ffontsize_r" ffontsize) :double
  "Bound to pl_ffontsize_r."
  (plotter :pointer)
  (size :double))

(defcfun ("pl_flabelwidth_r" flabelwidth) :double
  "Bound to pl_flabelwidth_r."
  (plotter :pointer)
  (s :string))

(defcfun ("pl_ftextangle_r" ftextangle) :double
  "Bound to pl_ftextangle_r."
  (plotter :pointer)
  (angle :double))

(defcfun ("pl_farc_r" farc) :int
  "Bound to pl_farc_r."
  (plotter :pointer)
  (xc :double)
  (yc :double)
  (x0 :double)
  (y0 :double)
  (x1 :double)
  (y1 :double))

(defcfun ("pl_farcrel_r" farcrel) :int
  "Bound to pl_farcrel_r."
  (plotter :pointer)
  (dxc :double)
  (dyc :double)
  (dx0 :double)
  (dy0 :double)
  (dx1 :double)
  (dy1 :double))

(defcfun ("pl_fbezier2_r" fbezier2) :int
  "Bound to pl_fbezier2_r."
  (plotter :pointer)
  (x0 :double)
  (y0 :double)
  (x1 :double)
  (y1 :double)
  (x2 :double)
  (y2 :double))

(defcfun ("pl_fbezier2rel_r" fbezier2rel) :int
  "Bound to pl_fbezier2rel_r."
  (plotter :pointer)
  (dx0 :double)
  (dy0 :double)
  (dx1 :double)
  (dy1 :double)
  (dx2 :double)
  (dy2 :double))

(defcfun ("pl_fbezier3_r" fbezier3) :int
  "Bound to pl_fbezier3_r."
  (plotter :pointer)
  (x0 :double)
  (y0 :double)
  (x1 :double)
  (y1 :double)
  (x2 :double)
  (y2 :double)
  (x3 :double)
  (y3 :double))

(defcfun ("pl_fbezier3rel_r" fbezier3rel) :int
  "Bound to pl_fbezier3rel_r."
  (plotter :pointer)
  (dx0 :double)
  (dy0 :double)
  (dx1 :double)
  (dy1 :double)
  (dx2 :double)
  (dy2 :double)
  (dx3 :double)
  (dy3 :double))

(defcfun ("pl_fbox_r" fbox) :int
  "Bound to pl_fbox_r."
  (plotter :pointer)
  (x0 :double)
  (y0 :double)
  (x1 :double)
  (y1 :double))

(defcfun ("pl_fboxrel_r" fboxrel) :int
  "Bound to pl_fboxrel_r."
  (plotter :pointer)
  (dx0 :double)
  (dy0 :double)
  (dx1 :double)
  (dy1 :double))

(defcfun ("pl_fcircle_r" fcircle) :int
  "Bound to pl_fcircle_r."
  (plotter :pointer)
  (x :double)
  (y :double)
  (r :double))

(defcfun ("pl_fcirclerel_r" fcirclerel) :int
  "Bound to pl_fcirclerel_r."
  (plotter :pointer)
  (dx :double)
  (dy :double)
  (r :double))

(defcfun ("pl_fcont_r" fcont) :int
  "Bound to pl_fcont_r."
  (plotter :pointer)
  (x :double)
  (y :double))

(defcfun ("pl_fcontrel_r" fcontrel) :int
  "Bound to pl_fcontrel_r."
  (plotter :pointer)
  (x :double)
  (y :double))

(defcfun ("pl_fellarc_r" fellarc) :int
  "Bound to pl_fellarc_r."
  (plotter :pointer)
  (xc :double)
  (yc :double)
  (x0 :double)
  (y0 :double)
  (x1 :double)
  (y1 :double))

(defcfun ("pl_fellarcrel_r" fellarcrel) :int
  "Bound to pl_fellarcrel_r."
  (plotter :pointer)
  (dxc :double)
  (dyc :double)
  (dx0 :double)
  (dy0 :double)
  (dx1 :double)
  (dy1 :double))

(defcfun ("pl_fellipse_r" fellipse) :int
  "Bound to pl_fellipse_r."
  (plotter :pointer)
  (x :double)
  (y :double)
  (rx :double)
  (ry :double)
  (angle :double))

(defcfun ("pl_fellipserel_r" fellipserel) :int
  "Bound to pl_fellipserel_r."
  (plotter :pointer)
  (dx :double)
  (dy :double)
  (rx :double)
  (ry :double)
  (angle :double))

(defcfun ("pl_flinedash_r" flinedash) :int
  "Bound to pl_flinedash_r."
  (plotter :pointer)
  (n :int)
  (dashes :pointer)
  (offset :double))

(defcfun ("pl_fline_r" fline) :int
  "Bound to pl_fline_r."
  (plotter :pointer)
  (x0 :double)
  (y0 :double)
  (x1 :double)
  (y1 :double))

(defcfun ("pl_flinerel_r" flinerel) :int
  "Bound to pl_flinerel_r."
  (plotter :pointer)
  (dxc :double)
  (dyc :double)
  (dx0 :double)
  (dy0 :double)
  (dx1 :double)
  (dy1 :double))

(defcfun ("pl_flinewidth_r" flinewidth) :int
  "Bound to pl_flinewidth_r."
  (plotter :pointer)
  (size :double))

(defcfun ("pl_fmarker_r" fmarker) :int
  "Bound to pl_fmarker_r."
  (plotter :pointer)
  (x :double)
  (y :double)
  (type :int)
  (size :double))

(defcfun ("pl_fmarkerrel_r" fmarkerrel) :int
  "Bound to pl_fmarkerrel_r."
  (plotter :pointer)
  (dx :double)
  (dy :double)
  (type :int)
  (size :double))

(defcfun ("pl_fmove_r" fmove) :int
  "Bound to pl_fmove_r."
  (plotter :pointer)
  (x :double)
  (y :double))

(defcfun ("pl_fmoverel_r" fmoverel) :int
  "Bound to pl_fmoverel_r."
  (plotter :pointer)
  (x :double)
  (y :double))

(defcfun ("pl_fpoint_r" fpoint) :int
  "Bound to pl_fpoint_r."
  (plotter :pointer)
  (x :double)
  (y :double))

(defcfun ("pl_fpointrel_r" fpointrel) :int
  "Bound to pl_fpointrel_r."
  (plotter :pointer)
  (dx :double)
  (dy :double))

(defcfun ("pl_fspace_r" lp-fspace) :int
  "Bound to pl_fspace_r."
  (plotter :pointer)
  (x0 :double)
  (y0 :double)
  (x1 :double)
  (y1 :double))

(defcfun ("pl_fspace2_r" lp-fspace2) :int
  "Bound to pl_fspace2_r."
  (plotter :pointer)
  (x0 :double)
  (y0 :double)
  (x1 :double)
  (y1 :double)
  (x2 :double)
  (y2 :double))

(defcfun ("pl_fconcat_r" fconcat) :int
  "Bound to pl_fconcat_r."
  (plotter :pointer)
  (m0 :double)
  (m1 :double)
  (m2 :double)
  (m3 :double)
  (m4 :double)
  (m5 :double))

(defcfun ("pl_fmiterlimit_r" fmiterlimit) :int
  "Bound to pl_fmiterlimit_r."
  (plotter :pointer)
  (limit :double))

(defcfun ("pl_frotate_r" frotate) :int
  "Bound to pl_frotate_r."
  (plotter :pointer)
  (theta :double))

(defcfun ("pl_fscale_r" fscale) :int
  "Bound to pl_fscale_r."
  (plotter :pointer)
  (x :double)
  (y :double))

(defcfun ("pl_fsetmatrix_r" setmatrix) :int
  "Bound to pl_fsetmatrix_r."
  (plotter :pointer)
  (m0 :double)
  (m1 :double)
  (m2 :double)
  (m3 :double)
  (m4 :double)
  (m5 :double))

(defcfun ("pl_ftranslate_r" ftranslate) :int
  "Bound to pl_ftranslate_r."
  (plotter :pointer)
  (x :double)
  (y :double))
