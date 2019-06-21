;|  Pline3Dto2D.lsp [command name: 3P2]
To convert 3D Polyline(s) into 2-dimensional ["heavy" or LightWeight] Polyline(s).
Terminology in comments:  "3D" refers to 3D Polyline entity [even if flat/planar, whether
  in or parallel to current drawing plane or in other UCS], "2D" refers only to "heavy" 2D
  Polyline, "LW" to LightWeight Polyline.
Draws at Elevation 0 in current UCS XY plane [not current view if other than Plan view].
Draws as LW or 2D depending on current PLINETYPE System Variable setting [always
  2D if original is spline-curved].
If 3D original is spline-curved, FLATTEN command with answer Yes to "Remove hidden
  lines?" option makes into LW of many line segments, whereas 3P2 makes true spline-
  curved 2D built on original's definition points flattened into current plane.
3D original will display neither non-continuous linetype assigned to its Layer nor any
  override linetype, but can have one by either means; if so, 2D/LW result will have it.
Kent Cooper, 6 March 2017
|;

(defun C:3P2 ; = 3d Polyline -> 2d
  (/ *error* doc svnames svvals ss n 3Dent 3Dobj curv 3Dcoords 2Dverts pt 2Dobj)

  (defun *error* (errmsg)
    (if (not (wcmatch errmsg "Function cancelled,quit / exit abort,console break"))
      (princ (strcat "\nError: " errmsg))
    ); if
    (mapcar 'setvar svnames svvals); reset System Variables
    (vla-endundomark doc)
    (princ)
  ); defun - *error*

  (vla-startundomark (setq doc (vla-get-activedocument (vlax-get-acad-object))))
  (setq ; System Variable saving/resetting without separate variables for each:
    svnames '(cmdecho blipmode plinetype)
    svvals (mapcar 'getvar svnames)
  ); setq
  (mapcar 'setvar svnames '(0 0)); turn off command echoing, blips

  (if (setq ss (ssget "_:L" '((0 . "POLYLINE") (-4 . "&") (70 . 8))))
    ;; 3D Polylines only, on unlocked Layers, open or closed, Spline-curved or not
    (repeat (setq n (sslength ss))
      (setq
        3Dent (ssname ss (setq n (1- n)))
        3Dobj (vlax-ename->vla-object 3Dent)
        curv (> (vla-get-Type 3Dobj) 0); spline-curved [T/nil]
        2Dverts nil ; eliminate previous if any
      ); setq
      (repeat (/ (length (setq 3Dcoords (vlax-get 3Dobj 'Coordinates))) 3)
        (setq
          2Dverts
            (cons
              (reverse (cdr (reverse ; XY only of:
                (trans (list (car 3Dcoords) (cadr 3Dcoords) (caddr 3Dcoords)) 0 1)
                ;; for correct outcome when done in non-World UCS, must (trans)late
                ;; each vertex with Z coordinate included, remove that from result
              ))); reverse/cdr/reverse
              2Dverts
            ); cons
          3Dcoords (cdddr 3Dcoords); remove this vertex's coordinates
        ); setq
      ); repeat
      (if curv (setvar 'plinetype 0)); draw as 2D "heavy" for applying 'Type
        ;; [because for some reason, drawing LW then converting to "heavy" later
        ;;   won't allow assignment of 'Type property within routine, as if still LW]
      (command "_.pline"); [LW or 2D by PLINETYPE System Variable setting]
      (foreach pt 2Dverts (command "_none" pt)); draw it
      (command ""); end Pline
      (vla-put-Closed (setq 2Dobj (vlax-ename->vla-object (entlast))) (vla-get-closed 3Dobj))
      (if curv (vla-put-Type 2Dobj (1+ (vla-get-Type 3Dobj)))); spline-curved
        ;; splined 3D value [quadratic/cubic] adjusted to corresponding 2D value
      (command "_.matchprop" 3Dent (entlast) ""); Layer, any override color/linetype/etc.
      (entdel 3Dent); remove original
      (if curv (setvar 'plinetype (last svvals))); reset for next if needed
    ); repeat
  ); if
  (mapcar 'setvar svnames svvals); reset
  (vla-endundomark doc)
  (princ)
); defun -- C:3P2

(vl-load-com)
(prompt "\nType 3P2 to convert 3D Polyline to 2D/LW.")