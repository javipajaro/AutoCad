;;;;-------------------------------------------------------------------------------
;;;; Program Name: ECoor.lsp [Extraction Coordinates]
;;;; Created By:   Vladimir Azarko (VVA) (Email: azarko@yandex.ru)
;;;; Posted http://www.cadtutor.net/forum/showthread.php?t=31653
;;;; Post: http://www.cadtutor.net/forum/showthread.php?31653-Lisp-coordinates-of-points-to-excel-sheet-(-point-number)&p=258931&viewfull=1#post258931
;;;; Date Created: I do not remember when the beginnings
;;;; Function:     Export coordinates with some options
;;;; Settings: find *ptcol:DELTA* and *ptcol:TEXTSIZE* below
;;;;                or use options Settings
;;; Commands:
;;;           COOR  - export coordinates
;;;           COORN - export coordinates with numbering
;;;                   Numbers of points are drawn by the text on the current layer,
;;;                   the current style, text height find *ptcol:TEXTSIZE*  or use options Settings
;;;           COORNP - CoorNP-Coordinates + Numbers + Points. Draw points with current value of PDMODE and PDSIZE
;;;           COORNB - CoorNB-Coordinates + Block (label(code)+Numbers + Points). Draw block with points and attrib with number Coordinates
;;;           COORT - export of coordinates with numbering where number considers the text nearest to a point
;;;;-------------------------------------------------------------------------------
;;;; Revision History
;;;; Rev  By     Date    Description
;;;;-------------------------------------------------------------------------------
;;;; 1    VVA   2009-02-09  Has started to put things in order
;;;; 2    VVA   2010-05-15 Add Elevation as Z coordinates from export of polyline
;;;; 3    VVA   2011-10-07 Add circle
;;;; 4    VVA   2012-02-27 Add command CoorNB and other minor change
;;;; 5    VVA   Add Settings options
;;;; 6    VVA   Add  Prefix and Suffix
(vl-load-com)
;;; =============== SETTINSG START ==================
(or *ptcol:DELTA*
(setq *ptcol:DELTA* ;;; offset the text from the point
       (list
         0.5 ;_Delta X
         0.5 ;_Delta Y
         0.0 ;_Delta Z
         )
      )
    )
(or *ptcol:TEXTSIZE*  ;;; TEXT HEIGHT
    (setq *ptcol:TEXTSIZE* (getvar "TEXTSIZE"))
    )
(or *ptcol:SEPARATE* ;;;SEPARATE COORDINATES FLAG
    (setq *ptcol:SEPARATE* "")
    )
(or *ptcol:PREF* ;;;Prefix
    (setq *ptcol:PREF* "")
    )
(or *ptcol:SUFF* ;;;Prefix
    (setq *ptcol:SUFF* "")
    )

;;; =============== SETTINSG END ==================


;;; =============== COMMAND SECTION START ==================

(defun c:COOR ( / ptLst)
  (vl-load-com)
  (if (setq ptLst (vva-get-final-points-list))
    (vva-export-point ptLst 0)
    )
  (princ)
  )
;|===============  оманда COORN ===============================================

EN:
   Export of coordinates of the specified points, the chosen objects: points, blocks, polylines, splines in a text file, Excel With numbers.
   Text file Ч txt, or csv. A rounding off of coordinates according to current adjustments of a command _UNITS (LUPREC !!!)
   And make P
|;

(defun c:COORN ( / ptLst)
  (vl-load-com)
  (if (setq ptLst (vva-get-final-points-list))
    (vva-export-point ptLst 1)
    )
  (princ)
  )
(defun c:COORNP ( / ptLst)
  (vl-load-com)
  (if (setq ptLst (vva-get-final-points-list))
    (vva-export-point ptLst 3)
    )
  (princ)
  )
(defun c:COORNB ( / ptLst)
  (vl-load-com)
  (if (setq ptLst (vva-get-final-points-list))
    (vva-export-point ptLst 4)
    )
  (princ)
  )
(defun c:COORT( /  ptLst objSet lw buf pat  oFlag sFlag txtList filPath cFile)
    (vl-load-com)
(if (setq ptLst (vva-get-final-points-list))
  (progn
    (setq objSet(ssget "_X" (list '(0 . "*TEXT")(cons 410 (getvar "CTAB")))))
    (setq lw (vl-remove-if 'listp(mapcar 'cadr(ssnamex objSet))))
    (setq lw (mapcar '(lambda(x)(setq x (entget x))(list (cdr(assoc 10 x))(cdr(assoc 1 x)))) lw))
    (foreach pt ptlst
      (setq pt (trans pt 1 0))
      (setq buf (mapcar '(lambda(x)(list (distance pt (car x))(cadr x))) lw))
      (setq pat (car buf))
      (foreach dst buf (if (< (car dst) (car pat))(setq pat dst)))
      (setq txtList (cons (cadr pat) txtList))
      )
    (setq txtList (reverse txtList))
    (princ "\n+++++++ Coordinates list +++++++\n")
    (setq buf
    (mapcar '(lambda (x y)
               (princ (strcat "\n" y "  "
                              (rtos (car x))
                              ","
                              (rtos (cadr x))
                              (if (= 3 (length x))
                                (strcat "," (rtos (nth 2 x)))
                                ""
                              ) ;_ end of if
                      ) ;_ end of strcat
               ) ;_ end of princ
              (list y (rtos (car x))(rtos (cadr x))
                              (if (= 3 (length x))(rtos (nth 2 x))) ;_ end of if
                      )
             ) ;_ end of lambda
            ptLst txtList
    );_ end mapcar
          )
    (princ "\n\n+++++++++ End of list +++++++++")
  (initget
    "Text Excel Not _Text Excel Not"
  ) ;_ end of initget
  (setq sFlag
         (getkword
             "\nSave coordinates to [Txt or csv file/Excel/Not save] <Text> : "
         ) ;_ end of getkword
  ) ;_ end of setq
    (if (null sFlag)
      (setq sFlag "Text")
    ) ;_ end of if
    (cond ((and (= "Text" sFlag)
              (setq filPath
                     (getfiled "Save Coordinates to Text or CSV File"
                               "Coordinates.csv"
                               "csv;txt"
                               17
                     ) ;_ end of getfiled
              ) ;_ end of setq
         ) ;_ end of and
         (if (= (strcase (vl-filename-extension filPath)) ".CSV")
           (setq pat ";")
           (setq pat "      ")
         ) ;_ end of if
         (if (or (not (findfile filPath))
                 (and  (findfile filPath)
                       (VL-FILE-SYSTIME filPath)
                       )
                 )
           (progn
         (setq cFile (open filPath "w"))
         (foreach ln buf
             (write-line
               (apply 'strcat
               (append (list(car ln))
                       (mapcar '(lambda(x)(strcat "," x))
                               (cdr ln)
                               )
                       )
                 )     
               cFile
             ) ;_ end of write-line
           )
         (close cFile)
         (initget "Yes No")
         (setq oFlag (getkword "\nOpen text file? [Yes/No] <No> : "
                     ) ;_ end of getkword
         ) ;_ end of setq
         (if (= oFlag "Yes")
           ;;; (startapp "notepad.exe" filPath)
           (vva-xopen filPath)
         ) ;_ end of if
         )
           (alert (strcat "Do not open" "\n" filPath))
           )
        )
                                               ; end condition #1
          ((= "Excel" sFlag)
           (xls buf
                  '("Number" "X" "Y" "Z")
                nil
                "COORT"
           ) ;_ end of xls
          )                                       ; end condition #2
          (t nil)
    ) ;_ end of cond
  ) ;_ end of progn
) ;_ end of if
 (princ))

;;; =============== COMMAND SECTION END ==================

;;; ==================== Helper function Start ===========================
;|================== XLS ========================================
*  published http://www.autocad.ru/cgi-bin/f1/board.cgi?t=31371zf
               http://forum.dwg.ru/showthread.php?t=20509
* Purpose: Export of the list of data Data-list in Excell
*             It is exported to a new leaf of the current book.
              If the book is not present, it is created
* Arguments:
              Data-list Ч The list of lists of data (LIST)
                            ((Value1 Value2 ... VlalueN)(Value1 Value2 ... VlalueN)...)
                            Each list of a kind (Value1 Value2... VlalueN) enters the name in
                            a separate line in corresponding columns (Value1-A Value2-B and .т.д.)
                  header Ч  The list (LIST) headings or nil a kind (" Signature A " " Signature B "...)
                            If header nil, is accepted ("X" "Y" "Z")
                 Colhide Ч  The list of alphabetic names of columns to hide or nil Ч to not hide ("A" "C" "D") Ч to hide columns A, C, D
                 Name_list Ч The name of a new leaf of the active book or nil Ч is not present
* Return: nil
* Usage
(xls '((1.1 1.2 1.3 1.4)(2.1 2.2 2.3 2.4)(3.1 3.2 3.3 3.4)) '("Col1" "Col2" "Col3"  "Col4") '("B") "test")   |;


(vl-load-com)
(defun xls ( Data-list header Colhide Name_list / *aplexcel* *books-colection* Currsep
*excell-cells* *new-book* *sheet#1* *sheet-collection* col iz_listo row cell cols)
(defun Letter (N / Res TMP)(setq Res "")(while (> N 0)(setq TMP (rem N 26)
  TMP (if (zerop TMP)(setq N (1- N) TMP 26) TMP)
  Res (strcat (chr (+ 64 TMP)) Res)  N   (/ N 26))) Res)
(if (null Name_list)(setq Name_list ""))
  (setq  *AplExcel*     (vlax-get-or-create-object "Excel.Application"))
  (if (setq *New-Book*  (vlax-get-property *AplExcel* "ActiveWorkbook"))
    (setq *Books-Colection*  (vlax-get-property *AplExcel* "Workbooks")
          *Sheet-Collection* (vlax-get-property *New-Book* "Sheets")
               *Sheet#1*     (vlax-invoke-method *Sheet-Collection* "Add"))
(setq *Books-Colection*  (vlax-get-property *AplExcel* "Workbooks")
              *New-Book*     (vlax-invoke-method *Books-Colection* "Add")
          *Sheet-Collection* (vlax-get-property *New-Book* "Sheets")
               *Sheet#1*     (vlax-get-property *Sheet-Collection* "Item" 1)))
(setq *excell-cells*     (vlax-get-property *Sheet#1* "Cells"))
(setq Name_list (if (= Name_list "")
                  (vl-filename-base(getvar "DWGNAME"))
                  (strcat (vl-filename-base(getvar "DWGNAME")) "&" Name_list))
   col 0 cols nil)
(if (> (strlen Name_list) 26)
(setq Name_list (strcat (substr Name_list 1 10) "..." (substr Name_list (- (strlen Name_list) 13) 14))))
(vlax-for sh *Sheet-Collection* (setq cols (cons (strcase(vlax-get-property sh 'Name)) cols)))
(setq row Name_list)
(while (member (strcase row) cols)(setq row (strcat Name_list " (" (itoa(setq col (1+ col)))")")))
(setq Name_list row)
(vlax-put-property *Sheet#1* 'Name Name_list)
(setq Currsep (vlax-get-property *AplExcel* "UseSystemSeparators"))
(vlax-put-property *AplExcel* "UseSystemSeparators" :vlax-false) 
(vlax-put-property *AplExcel* "DecimalSeparator" ".")            
(vlax-put-property *AplExcel* "ThousandsSeparator" " ")          
(vla-put-visible *AplExcel* :vlax-true)(setq row 1 col 1)
(if (null header)(setq header '("X" "Y" "Z")))
(repeat (length header)(vlax-put-property *excell-cells* "Item" row col
(vl-princ-to-string (nth (1- col) header)))(setq col (1+ col)))(setq  row 2 col 1)
(repeat (length Data-list)(setq iz_listo (car Data-list))(repeat (length iz_listo)
(vlax-put-property *excell-cells* "Item" row col (vl-princ-to-string (car iz_listo)))
(setq iz_listo (cdr iz_listo) col (1+ col)))(setq Data-list (cdr Data-list))(setq col 1 row (1+ row)))
(setq col (1+(length header)) row (1+ row))
(setq cell (vlax-variant-value (vlax-invoke-method *Sheet#1* "Evaluate"
    (strcat "A1:" (letter col)(itoa row))))) ;_ end of setq
(setq cols (vlax-get-property cell  'Columns))
(vlax-invoke-method cols 'Autofit)
(vlax-release-object cols)(vlax-release-object cell)
(foreach item ColHide (if (numberp item)(setq item (letter item)))
(setq cell (vlax-variant-value (vlax-invoke-method *Sheet#1* "Evaluate"
    (strcat item "1:" item "1"))))
(setq cols (vlax-get-property cell  'Columns))
(vlax-put-property cols 'hidden 1)
(vlax-release-object cols)(vlax-release-object cell))
(vlax-put-property *AplExcel* "UseSystemSeparators" Currsep)
(mapcar 'vlax-release-object (list *excell-cells* *Sheet#1* *Sheet-Collection* *New-Book* *Books-Colection*
*AplExcel*))(setq *AplExcel* nil)(gc)(gc)(princ))
(defun group-by-num (lst num / ls ret)(if (= (rem (length lst) num ) 0)(progn (setq ls nil)
  (repeat (/ (length lst) num)(repeat num (setq ls(cons (car lst) ls)lst (cdr lst)))
  (setq ret (append ret (list (reverse ls))) ls nil)))) ret)
(defun PtCollect(SelSet)(mapcar 'cdr (mapcar '(lambda(x)(assoc 10 x))(mapcar 'entget
(vl-remove-if 'listp(mapcar 'cadr(ssnamex SelSet))))))); end of PtCollect
(defun PLCollect(SelSet / ret tmp)
(foreach lw (mapcar 'vlax-ename->vla-object (vl-remove-if 'listp(mapcar 'cadr(ssnamex SelSet))))
  (cond ((wcmatch (vla-get-ObjectName lw) "*Polyline")
	 (setq tmp (group-by-num (vlax-get lw 'Coordinates)
		     (if (=(vla-get-ObjectName lw) "AcDbPolyline") 2 3)))
	 (if (vlax-property-available-p lw 'Elevation)
         (setq tmp(mapcar '(lambda(x)(list (car x)(cadr x)(vla-get-elevation lw))) tmp)))
	 (setq ret (append ret tmp)))
       ((=(vla-get-ObjectName lw) "AcDbSpline")(setq ret (append ret (group-by-num
         (vlax-safearray->list(vlax-variant-value (vla-get-controlpoints lw)))  3))))
      (t nil))
  (if (not(eq *ptcol:SEPARATE* ""))(setq ret (append ret (list *ptcol:SEPARATE*))))
  )
  ret)
(defun vva-text-draw (txt pnt height rotation justification)
   (if (null pnt)(command "_.-TEXT" "" txt)
   (if (= (cdr (assoc 40 (tblsearch "STYLE" (getvar "TEXTSTYLE"))))
    0.0
       ) ;_ end of =
     (progn
     ;; нулева€ высота текста
       (if justification
   (command "_.-TEXT" "_J" justification "_none" pnt height rotation txt)
   (command "_.-TEXT" "_none" pnt height rotation txt)
       ) ;_ end of if
     ) ;_ end of progn
     (progn
       (if justification
   (command "_.-TEXT" "_J" justification "_none" pnt rotation txt)
   (command "_.-TEXT" "_none" pnt rotation txt)
       ) ;_ end of if
     ) ;_ end of progn
   ) ;_ end of if
     )
  (entlast)
)

(defun vva-conv-ent-to-ename (ent_value / ret)
;;;(vva-conv-ent-to-ename (entlast))
;;;(vva-conv-ent-to-ename (entget(entlast)))
;;;(vva-conv-ent-to-ename (cdr(assoc 5 (entget(entlast)))))
;;;(vva-conv-ent-to-ename (car(entsel)))
;;;(vva-conv-ent-to-ename (vlax-ename->vla-object (entlast)))

  (cond
    ((= (type ent_value) 'vla-object) (vlax-vla-object->ename ent_value))
    ((= (type ent_value) 'ename) ent_value)
    ((and (= (type ent_value) 'list)
          (= (type (setq ret (car ent_value))) 'ename)
          )
     ret
     )
    ((and (= (type ent_value) 'str)(setq ret (handent ent_value))) ret)
    ((= (type ent_value) 'list)(cdr (assoc -1 ent_value)))
    (t nil)
    ) ;_ end of cond
  ) ;_ end of defun



(defun vva-pl-get-coors&bulge ( pl / ent_data tmp_ent blglist coors Z)
;;; pl - vla or ename polyline  
;;; pl-ename or vla object
;;; Return lists coordinates and bulge polyline
;;; Return list of 2 list
;;; 1-st list  - coordinate (WCS)
;;; 2-nd list  - bulge
  
(setq pl (vva-conv-ent-to-ename PL))  
  (setq ent_data (entget pl))
  (if (= (cdr(assoc 0 ent_data))  "LWPOLYLINE")
    (progn
      (setq Z (cdr(assoc 38 ent_data)))
    (foreach lst ent_data
      (setq num (car lst))
      (cond
        ((= num 10)(setq coors (cons (append (cdr lst)(list Z)) coors)))
        ((= num 42)(setq blglist (cons (cdr lst) blglist)))
        (t nil)
        )
      )
    )
    (progn
      (setq tmp_ent pl)
      (while (/= "SEQEND" (cdr(assoc 0 (setq ent_data (entget(setq tmp_ent (entnext tmp_ent)))))))
             (setq coors (cons (cdr (assoc 10 ent_data))  coors))
             (setq blglist (cons (cdr (assoc 42 ent_data)) blglist))
	   );_while
      )
    )
  (list (reverse coors)
        (reverse blglist)
        )
  )
(defun vva-conv-to-str (dat)
  (cond ((= (type dat) 'INT)(setq dat (itoa dat)))
         ((= (type dat) 'REAL)(setq dat (rtos dat 2 12)))
        ((null dat)(setq dat ""))
        (t (setq dat (vl-princ-to-string dat)))))
(defun vva-put-hyperlink (ENAME URLDescription)
;;; mip-put-hyperlink
;;; Return nil.
;;; Arguments [Type]:
;;;   Ename = Object [ENAME or VLA]
;;;  URLDescription = String [STR]
;;; If URLDescription="", then delete hyperlink
  (if (eq (type ENAME) 'ENAME)
    (setq ENAME (vlax-ename->vla-object ENAME))
  ) ;_ end of if
  (vlax-for hyp (vla-get-hyperlinks ENAME) (vla-delete hyp))
  (if (/= URLDescription "")
  (vla-add (vla-get-hyperlinks ENAME)
           "about:blank"
           URLDescription
  ) ;_ end of vla-add
    )
) ;_ end of DEFUN

(defun vva-hvac-rep ( / objSet PLList FinalList NodeList point Node segment tmp pat bulge skip)
  (if (zerop (getvar "PDMODE"))(setvar "PDMODE" 35))
    (princ "\nSelect polyline and press Enter ")
    (setq objSet (ssget '((0 . "*POLYLINE"))))
    (setq PLList (vl-remove-if 'listp(mapcar 'cadr(ssnamex objSet))))
    (setq Finallist nil)
    (foreach pl PLList
      ;;;(setq pl (nth 1 pllist))
      (setq point (vva-pl-get-coors&bulge pl)
            bulge (cadr point) ;;; bulge
            point (car point)
            )
       (if (eq (vla-get-closed (vlax-ename->vla-object pl)) :vlax-true)
         (setq point (append point (list (car point)))
               bulge (append bulge (list (car bulge)))
         )
         )
      (setq Node 0 NodeList nil skip nil)
      (foreach pt point
        (if (and (zerop (nth Node bulge))(null skip))
              (setq Nodelist (cons (append pt '(0)) NodeList) skip nil)
          )
        (setq skip nil)
        (if (not(zerop (nth Node bulge)))
          (progn
            (setq skip t)
            ((lambda (bulge p1 p2 / delta chord radius center vx t1 t2)
                 (setq delta  (* (atan bulge) 4)
                       chord  (distance p1 p2)
                       radius (/ chord (sin (/ delta 2)) 2)
                       center (polar p1 (+ (angle p1 p2) (/ (- pi delta) 2)) radius)
                       t1 (polar p1 (+ (angle center p1)(* 0.5 PI)) 10)
                       t2 (polar p2 (+ (angle center p2)(* 0.5 PI)) 10)
                       vx (inters p1 t1 p2 t2 nil)
                       )
                 (setq tmp
                        (list
                          
                          (nth 0 vx)     ;_ X
                          (nth 1 vx)     ;_ Y
                          (nth 2 vx)     ;_ Z
                          (abs radius)   ;_Radius
                          )
                       )
                 )
                (nth Node bulge)
                (nth Node point)
                (nth (1+ Node) point)
                )
            (setq Nodelist (cons tmp NodeList))
            )
          )
        (setq Node (1+ Node))
        )
      (setq Node 0)
      (setq NodeList (mapcar '(lambda(x)(cons (setq Node (1+ Node)) x)) (reverse NodeList)))
      (setq FinalList (cons (list pl  NodeList) Finallist))

;;;      
;;;      (setq segment
;;;             (mapcar '(lambda( p1 p2 bulge)
;;;                 (list p1 p2 bulge)
;;;                 )
;;;              point (cdr point) tmp
;;;              )
;;;            )
;;;      (setq Node 0 NodeList nil)
;;;      (foreach seg segment
;;;        ;; (setq seg (nth 1 segment))
;;;        (if (zerop (nth 2 seg)) ;;;bulge
;;;          (progn
;;;              (setq 
;;;                 tmp (list 
;;;                          (nth 0 (car seg))     ;_ X
;;;                          (nth 1 (car seg))     ;_ Y
;;;                          (nth 2 (car seg))     ;_ Z
;;;                          0                     ;_Radius
;;;                          )
;;;                )
;;;            )
;;;          (progn
;;;              ((lambda (bulge p1 p2 / delta chord radius center vx)
;;;                 (setq delta  (* (atan bulge) 4)
;;;                       chord  (distance p1 p2)
;;;                       radius (/ chord (sin (/ delta 2)) 2)
;;;                       center (polar p1 (+ (angle p1 p2) (/ (- pi delta) 2)) radius)
;;;                       vx (polar p1 (angle p1 p2)(* 0.5 chord))
;;;                       vx (polar center (angle center vx)(* 2 (distance center vx)))
;;;                       )
;;;                 (setq tmp
;;;                        (list
;;;                          
;;;                          (nth 0 vx)     ;_ X
;;;                          (nth 1 vx)     ;_ Y
;;;                          (nth 2 vx)     ;_ Z
;;;                          radius               ;_Radius
;;;                          )
;;;                       )
;;;                 )
;;;                (nth 2 seg)
;;;                (car seg)
;;;                (cadr seg)
;;;                )
;;;            )
;;;          )
;;;        (setq NodeList (cons tmp NodeList))
;;;        )
;;;      (setq segment(reverse NodeList) NodeList nil)
;;;      (setq Node 0)
;;;      (foreach seg segment
;;;        (if (zerop
;;;        
;;;        
;;;        )
;;;      (foreach seg segment
;;;        ;; (setq seg (nth 1 segment))
;;;        (if (zerop (nth 2 seg)) ;;;bulge
;;;          (progn
;;;            (if pat (setq tmp (cadr seg))(setq tmp (car seg)))
;;;            (setq pat nil)
;;;              (setq 
;;;                 tmp (list (setq Node (1+ Node)) ;_ Nodenum
;;;                          (nth 0 tmp)     ;_ X
;;;                          (nth 1 tmp)     ;_ Y
;;;                          (nth 2 tmp)     ;_ Z
;;;                          0                     ;_Radius
;;;                          )
;;;                )
;;;            )
;;;          (progn
;;;              ((lambda (bulge p1 p2 / delta chord radius center vx)
;;;                 (setq delta  (* (atan bulge) 4)
;;;                       chord  (distance p1 p2)
;;;                       radius (/ chord (sin (/ delta 2)) 2)
;;;                       center (polar p1 (+ (angle p1 p2) (/ (- pi delta) 2)) radius)
;;;                       vx (polar p1 (angle p1 p2)(* 0.5 chord))
;;;                       vx (polar center (angle center vx)(* 2 (distance center vx)))
;;;                       )
;;;                 (setq pat vx)
;;;                 (setq tmp
;;;                        (list
;;;                          (setq Node (1+ Node)) ;_ Nodenum
;;;                          (nth 0 vx)     ;_ X
;;;                          (nth 1 vx)     ;_ Y
;;;                          (nth 2 vx)     ;_ Z
;;;                          radius               ;_Radius
;;;                          )
;;;                       )
;;;                 )
;;;                (nth 2 seg)
;;;                (car seg)
;;;                (cadr seg)
;;;                )
;;;            )
;;;          )
;;;        (setq NodeList (cons tmp NodeList))
;;;        )
;;;      (if (= (length segment) 1)
;;;        (progn
;;;          (setq
;;;             tmp (nth 1 (car segment))
;;;             tmp (list (setq Node (1+ Node)) ;_ Nodenum
;;;                          (nth 0 tmp)     ;_ X
;;;                          (nth 1 tmp)     ;_ Y
;;;                          (nth 2 tmp)     ;_ Z
;;;                          0                     ;_Radius
;;;                          )
;;;                )
;;;          (setq NodeList (cons tmp NodeList))
;;;        )
;;;        )
      ;;;(setq NodeList (reverse NodeList))
;;;      (setq FinalList (cons (list pl (reverse NodeList)) Finallist))
      )
  (setq objSet nil)
  (if (and FinalList
           (setq tmp (getfiled "Save HVAC Duct Data to CSV File"
                      "HvacDuctDataReport.csv"
                      "csv"
                       1
                     ) ;_ end of getfiled
              )
           )
    (progn
      (setq Node 0) ;_ Polyline Number
       (if (or (not (findfile tmp))
                 (and  (findfile tmp)
                       (VL-FILE-SYSTIME tmp)
                       )
                 )
           (progn
         (setq NodeList (open tmp "w"))
         (setq pat ";")
         (write-line "Polyline;NodeNum;X-coord;Y-coord;Z-coord;Bend-Radius" NodeList)
         (foreach dat FinalList
           (setq Node (1+ Node))
           (setq point (vlax-ename->vla-object(car dat))) ;;; Vla object of polyline
           (if (vlax-write-enabled-p point)
             (vva-put-hyperlink point (strcat "Polyline N " (itoa Node)))
             )
           (foreach ln (cadr dat)
             (setq objSet
             (entmakex (list  '(0 . "TEXT")
                              '(100 . "AcDbEntity")
                              '(100 . "AcDbText")
                              (cons 8 "HVAC_Node_Text")  ;;;Layer for NodeNum Text
                              (list 10 (nth 1 ln)(nth 2 ln) 0.0)
                              (cons 1 (vva-conv-to-str (car ln)))
                              (cons 40 *ptcol:TEXTSIZE*)
                              (cons 7 (getvar "TEXTSTYLE"))
                              '(72 . 0)
                              '(50 . 0)
                              (list 11 (nth 1 ln)(nth 2 ln) 0.0)
                              ) ;_ end of list
                       )
                   )
             (vva-put-hyperlink objSet (strcat "Radius " (vva-conv-to-str (nth 4 ln))))
             (entmakex (list (cons 0 "POINT")(cons 8 "HVAC_Node_Point")(list 10 (nth 1 ln)(nth 2 ln) 0.0)))
             (setq ln (mapcar '(lambda(x / tmp)
                                 (setq tmp
                                 (VL-STRING-RIGHT-TRIM "."
                                 (VL-STRING-RIGHT-TRIM "0"
                                   (vva-conv-to-str x))))
                                 (if (= tmp "") "0" tmp)
                                       ) ln))
             (write-line
             (strcat (itoa Node)
                     pat
                     (car ln)
                     (apply 'strcat
                     (mapcar '(lambda(x)(strcat pat x))
                             (cdr ln)
                             )
                            )
                     )
             NodeList
             ) ;_ end of write-line
             )
           )
         (close Nodelist)
         (initget "Yes No")
         (setq point (getkword 
                                (strcat  "\nOpen "
                                         (VL-FILENAME-BASE tmp)
                                         (vl-filename-extension tmp)
                                         " file? [Yes/No] <No> : "
                                         )
                               
                     ) ;_ end of getkword
         ) ;_ end of setq
         (if (= point "Yes")
           ;;; (startapp "notepad.exe" filPath)
           (vva-xopen tmp)
         ) ;_ end of if
         )
           (alert (strcat  "Do not open" "\n" tmp))
           )
      )
    )
)
(defun C:HVACRep ()(vva-hvac-rep)(princ))
(defun vva-get-final-points-list ( / curPt ptLst objSet flag )
  (setq flag t)
  (if(not ptcol:mode)(setq ptcol:mode "Pick"))
  (while flag
    (initget "Pick pOints Blocks poLyline Circle Settings")
    (princ "\nSettings: Text offset=(")
    (princ (car *ptcol:DELTA*))
    (mapcar '(lambda(x)(princ ",")(princ x)) (cdr *ptcol:DELTA*))
    (princ ")")(princ " Text heigth=")(princ *ptcol:TEXTSIZE*)
    (princ " Separate coordinates flag=")
    (if (eq *ptcol:SEPARATE* "")(princ "none")
      (progn(princ "<")(princ *ptcol:SEPARATE*)(princ ">"))
      )
    (setq oldMode ptcol:mode
          ptcol:mode
           (getkword (strcat "\nSpecify mode [Pick/pOints/Blocks/Circle/poLyline or spline/Settings] <"ptcol:mode">: "))
          ptLst nil
          )
    (if(null ptcol:mode)(setq ptcol:mode oldMode))
    (if (eq ptcol:mode "Settings")
      (progn
        (princ "\nType offset the text from the point <")
        (princ (car *ptcol:DELTA*))
        (mapcar '(lambda(x)(princ ",")(princ x)) (cdr *ptcol:DELTA*))
        (princ "> :")
        (and
          (setq curPt (getpoint))
          (setq *ptcol:DELTA* curPt)
          )
        (vl-propagate '*ptcol:DELTA*)
        (princ "\nType text height <")(princ *ptcol:TEXTSIZE*)(princ "> :")
        (and
          (setq curPt (getdist))
          (setq *ptcol:TEXTSIZE* curPt)
          )
        (vl-propagate '*ptcol:TEXTSIZE*)
        (princ "\nType flag, which separates the coordinates or press ENTER - none: ")
        (setq *ptcol:SEPARATE* (getstring))
        (vl-propagate '*ptcol:SEPARATE*)
        )
      (setq flag nil)
      )
    )
(cond ((= "Pick" ptcol:mode)(setq curPt T)
       (while curPt (setq curPt(getpoint  "\nPick point or Enter to continue > "))
  (if curPt (setq ptLst(append ptLst(list (trans curPt 1 0))))))); end condition #1
      ((= "pOints" ptcol:mode)(if (not(setq objSet(ssget "_I" '((0 . "POINT")))))(progn
         (princ "\nSelect points and press Enter ")
   (setq objSet(ssget '((0 . "POINT"))))))(if objSet (setq ptLst(PtCollect objSet)))); end condition #2
      ((= "Blocks" ptcol:mode)(if (not(setq objSet(ssget "_I" '((0 . "INSERT")))))(progn
        (princ "\nSelect blocks and press Enter ")
     (setq objSet(ssget '((0 . "INSERT"))))))(if objSet (setq ptLst(PtCollect objSet)))); end condition #3
      ((= "Circle" ptcol:mode)(if (not(setq objSet(ssget "_I" '((0 . "CIRCLE")))))(progn
        (princ "\nSelect circle and press Enter ")
     (setq objSet(ssget '((0 . "CIRCLE"))))))(if objSet (setq ptLst(PtCollect objSet))))
      ((= "poLyline" ptcol:mode)(if (not(setq objSet(ssget "_I" '((0 . "*POLYLINE,SPLINE")))))(progn
        (princ "\nSelect polyline and press Enter ")
     (setq objSet(ssget '((0 . "*POLYLINE,SPLINE"))))))(if objSet (setq ptLst(PLCollect objSet)))); end condition #4
); end cond
(if ptLst (progn (princ "\n+++++++ Coordinates list +++++++\n")(setq ptLst (mapcar '(lambda(x)(if (listp x)(trans x 0 1) x)) ptLst))
(mapcar '(lambda(x)(princ (if (listp x)(strcat "\n"(rtos(car x))","(rtos(cadr x))
(if(= 3(length x))(strcat ","(rtos(nth 2 x))) ""))(strcat "\n" (vva-conv-to-str x))))) ptLst); end mapcar
(princ "\n\n+++++++++ End of list +++++++++")
            )
  )
 ptLst
  )

(defun vva-entmake-block ()
  ;;; (vva-entmake-block)
  (entmake
    (list
      (cons 0 "BLOCK")
      (cons 2 "NPOINT")
      (cons 8 "0")
      (cons 70 2)
      (cons 3 "")
      (list 10 0 0 0)
      )
    )
  (entmake
    (list
      (cons 0 "POINT")
      (cons 100 "AcDbEntity")
      (cons 8 "0")
      (cons 100 "AcDbPoint")
      (list 10 0 0 0)
      )
    )
  (entmake
    (list
      (cons 0 "ATTDEF")
      (cons 8 "0")
      (cons 370 0)
      (cons 7 "Standard")
      (cons 10 *ptcol:DELTA*);;; Delta X and Delta Y Attdef
      (cons 11 *ptcol:DELTA*);;; Delta X and Delta Y Attdef
      (cons 40 *ptcol:TEXTSIZE*) ;;;TextSize
      (cons 1 " ")
      (cons 3 "Number of point: ")
      (cons 2 "NPOINT")
      (cons 70 0)
      (cons 72 0)
      (cons 74 0)
      )
    )
(entmake
  (list
    (cons 0 "ENDBLK")
    (cons 8 "0")
    )
  )
  )
(defun vva-xopen (name / di na sh)
;; get from Patrick_35
;; http://www.theswamp.org/index.php?topic=29548.0 
;;;Usage 
;;;(setq my_file (vva-xopen "c:/test.txt")) 
;;;(setq my_file (vva-xopen "c:/test.avi")) 
;;;(setq my_file (vva-xopen "c:/test.3gp")) 

  (and   (setq name (findfile name)) 
   (setq sh (vlax-create-object "Shell.Application")) 
   (setq di (vlax-invoke sh 'Namespace (vl-filename-directory name))) 
   (setq na (vlax-invoke di 'parsename (strcat (vl-filename-base name) (vl-filename-extension name)))) 
   (vlax-invoke-method na 'invokeverbex "open") 
  ) 
  (vlax-release-object sh) 
  na 
)

(defun vva-export-point (ptlst Options / Npt sFlag oFlag Step pat)
  ;;; ptLst - points list
  ;;; Options - number
  ;;;              1 - numerate (if 2 - text, if 4 - attrib)
  ;;;              2 - create point
  ;;;              4 - create block
  
  ;;; isNum - t - numerate nil -not
  ;;; isPoint - t - create point nil - not
  
  (if (or (= 1 (logand 1 Options)) ;_isNum = t
          (= 4 (logand 4 Options)) ;_create block
          )
    (progn

(or *ptcol:PREF* ;;;Prefix
    (setq *ptcol:PREF* "")
    )
(or *ptcol:SUFF* ;;;Prefix
    (setq *ptcol:SUFF* "")
    )
    (setq Npt
    (getstring T
      (strcat "\nType prefix:(If you want to delete prefix type ' ' (press SPACE key on keyboard) <"*ptcol:PREF*"> :")))
  (cond
    ((eq "" Npt) nil)
    ((eq " " Npt)(setq *ptcol:PREF* ""))
    (t (setq *ptcol:PREF* Npt))
    )
    (setq Npt
    (getstring T
      (strcat "\nType suffix:(If you want to delete prefix type ' ' (press SPACE key on keyboard) <"*ptcol:SUFF*"> :")))
  (cond
    ((eq "" Npt) nil)
    ((eq " " Npt)(setq *ptcol:SUFF* ""))
    (t (setq *ptcol:SUFF* Npt))
    )
      (princ "\nStart number of points <")
      (if (not (numberp ptcol:Npt))
        (setq ptcol:Npt 1)
      ) ;_ end of if
      (princ ptcol:Npt)
      (princ ">: ")
      (setq Npt(getint))
      (if (null Npt)
        (setq Npt ptcol:Npt)
      ) ;_ end of if
      (setq ptcol:Npt Npt)
    )
    )
  (setq Step 1)
  (if (numberp Npt)
    (progn
      (princ "\nIncrement of number") ;_ end of princ
      (if (not (numberp ptcol:Step))
        (setq ptcol:Step 1)
      ) ;_ end of if
      (princ " <")
      (princ ptcol:Step)
      (princ ">: ")
      (setq Step (getint))
      (if (null Step)
        (setq Step ptcol:Step)
      ) ;_ end of if
      (setq ptcol:Step Step)
    ) ;_ end of progn
  ) ;_ end of if
  (initget
    "Text Csv Excel Not"
  ) ;_ end of initget
  (setq sFlag
         (getkword
             "\nSave coordinates to [Text file/Csv file/Excel/Not save] <Excel> : "
         ) ;_ end of getkword
  ) ;_ end of setq
  (if (null sFlag)
    (setq sFlag "Excel")
  ) ;_ end of if
  (setq oFlag Npt)
  (if (numberp Npt)
    (foreach ln ptlst
      (if (listp ln)
        (progn
          (if (= 1 (logand 1 Options))
          (vva-text-draw
            (strcat *ptcol:PREF* (itoa Npt) *ptcol:SUFF*)
            (trans (mapcar '+ ln *ptcol:DELTA*) 1 0)
            *ptcol:TEXTSIZE*
            0
            nil
            ) ;_ end of vva-text-draw
            )
          (if (= 2 (logand 2 Options))
            (entmakex (list (cons 0 "POINT") (cons 10 (trans ln 1 0))))
            )
          (if (= 4 (logand 4 Options));_make block
            (progn
              (if (not (tblsearch "Block" "NPOINT"))
                (vva-entmake-block)
                )
              (entmake (list (cons 0 "INSERT") ;***
               (cons 8 "0")
               (cons 66 1) ;***
               (cons 2 "NPOINT") ;***
               (cons 10 (trans ln 1 0)) ;***
               (cons 41 1)
               (cons 42 1)
               (cons 50 0)
               (cons 43 1)
               (cons 70 0)
               (cons 71 0)
               (cons 44 0)
               (cons 45 0)
               (cons 210 (list 0 0 1))
               (cons 62 256)
               (cons 39 0)
               (cons 6 "BYLAYER")))
              (entmake (list (cons 0 "ATTRIB") ;***
               (cons 8 "0")
               (cons 10 (trans (mapcar '+ ln *ptcol:DELTA*) 1 0)) ;***
               (cons 40 1) ;***
               (cons 1 (strcat *ptcol:PREF* (itoa Npt) *ptcol:SUFF*)) ;***
               (cons 2 "NPOINT") ;***
               (cons 70 0)
               (cons 73 0)
               (cons 50 0)
               (cons 41 1)
               (cons 51 0)
               (cons 7 "STANDARD") ;***
               (cons 71 0)
               (cons 72 0)
               (cons 11 (trans (mapcar '+ ln *ptcol:DELTA*) 1 0)) ;***
               (cons 210 (list 0 0 1))
               (cons 74 0)
               (cons 62 256)
               (cons 39 0)
               (cons 6 "BYLAYER")))
              (entmake (list (cons 0 "SEQEND") ;***
               (cons 8 "0")))
              )
            )
          (setq Npt (+ Npt Step))
          )
        );_if listp ln
    ) ;_ end of foreach
  ) ;_ end of if
  (setq ptcol:Npt Npt Npt oFlag)
  (setq ptLst (mapcar '(lambda (x) (if (listp x)(mapcar 'rtos x)(vva-conv-to-str x))) ptlst)) ;_CCCXXX VVA-CONV-TO-STR
  (cond ((and (or (eq "Text" sFlag)(eq "Csv" sFlag))
              (setq filPath
                     (getfiled "Save Coordinates to Text or CSV File"
                               (strcat "Coordinates" (if (= "Csv" sFlag) ".csv" ".txt")) 
                               (if (= "Csv" sFlag) "csv;txt" "txt;csv")
                               1
                     ) ;_ end of getfiled
              ) ;_ end of setq
         ) ;_ end of and
         (if (= (strcase (vl-filename-extension filPath)) ".CSV")
           (setq pat ";")
           (setq pat "      ")
         ) ;_ end of if
         (if (or (not (findfile filPath))
                 (and  (findfile filPath)
                       (VL-FILE-SYSTIME filPath)
                       )
                 )
           (progn
         (setq cFile (open filPath "w"))
         (foreach ln ptLst
           (if (listp ln)
             (progn
           (write-line
             (strcat (if (numberp Npt)
                       (strcat (strcat *ptcol:PREF* (itoa Npt) *ptcol:SUFF*) pat)
                       ""
                     ) ;_ end of if
                     (car ln)
                     pat
                     (cadr ln)
                     (if (= 3 (length ln))
                       (strcat pat (nth 2 ln))
                       ""
                     ) ;_ end of if
             ) ;_ end of strcat
             cFile
           ) ;_ end of write-line
             (if (numberp Npt)
             (setq Npt (+ Npt Step))
           ) ;_ end of if
           )
             (write-line ln cFile)
             )
         ) ;_ end of foreach
         (close cFile)
         (initget "Yes No")
         (setq oFlag (getkword 
                                (strcat  "\nOpen "
                                         (VL-FILENAME-BASE filPath)
                                         (vl-filename-extension filPath)
                                         " file? [Yes/No] <No> : "
                                         )
                               
                     ) ;_ end of getkword
         ) ;_ end of setq
         (if (= oFlag "Yes")
           ;;; (startapp "notepad.exe" filPath)
           (vva-xopen filPath)
         ) ;_ end of if
         )
           (alert (strcat  "Do not open" "\n" filPath))
           )
        )                                         ; end condition #1
        ((= "Excel" sFlag)
         (if (numberp Npt)
           (progn
             (setq Npt (- Npt Step))
             (setq ptlst
                    (mapcar '(lambda (x) (if (listp x)(cons  (strcat *ptcol:PREF* (itoa (setq Npt (+ Npt Step))) *ptcol:SUFF*) x) x))
                            ptlst
                    ) ;_ end of mapcar
             ) ;_ end of setq
             (xls (mapcar '(lambda(x)(if(listp x) x (list x))) ptlst) '("N" "X" "Y" "Z") nil "COORN")
           ) ;_ end of progn
           
           (xls (mapcar '(lambda(x)(if(listp x) x (list x))) ptlst) nil nil "COOR")
         ) ;_ end of if
        )                                         ; end condition #2
        (t nil)
  ) ;_ end of cond
  (princ)
) ;_ end of defun

(princ"\nCOOR  - export coordinates")
(princ"\nCOORN - export coordinates with numbering. Numbers of points are drawn by the")
(princ"\n        text on the current layer, the current style, text height or")
(princ"\n        use options Settings")
(princ"\nCOORNP -Coordinates + Numbers(with Prefix and Suffix) + Points. Draw points")
(princ"\n        with current value of PDMODE and PDSIZE")
(princ"\nCOORNB -Coordinates + Block (Prefix+Numbers+Suffix + Points). Draw block")
(princ"\n        with points and attrib with number of Coordinates") 
(princ"\nCOORT - export of coordinates with numbering where number considers")
(princ"\n        the text nearest to a point")
(princ "\nRev.6 Type HVACRep, Coor, CoorN, CoorNP, CoorNB or CoorT in command line ")
(princ)
