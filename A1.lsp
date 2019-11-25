;Javi Ruiz 26/08/2019
;a1 is for atomatic printing, you need DWG To PDF.pc3 and to repath where to print
;for multiple printing use the excel



;insert here your local path for printing PDF
;dont forget double \\, as example C:\\Users\\jaruiz1\\Desktop\\_PDF\\" Str ".pdf
(setq pdfpath "C:\\Users\\jaruiz1\\Desktop\\_PDF\\")





(defun C:a1 ()
(COMMAND "TILEMODE" "0")
(setq str1 (getvar "DWGNAME"))
(setq Str1 (substr Str1 1 20))
(setq str (getvar "clayout"))
(setq Str (substr Str 1 (- (strlen Str) 6)))
(setq str (strcat STR1 STR))
(setq p4 (strcat pdfpath Str ".pdf"))
(command "-plot" "y" "" "DWG To PDF.pc3" "iso full bleed A1 (841.00 x 594.00 mm)" "m" "l" "n" "e" "1:1" "c" "y" "TfNSW_COLOR_FULL" "Y" "n" "N" "n" P4 "N" "Y")
(princ)
)