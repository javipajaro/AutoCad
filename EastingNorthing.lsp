(defun c:enz (/ p x y z ptcoord textloc)
(while
(setq p (getpoint "
Pick Point: "))
(setq textloc (getpoint "
Pick Label Location: "))
(setq x (rtos (car p)))
(setq y (rtos (cadr p)))
(setq z (rtos (caddr p))) ;*you may delete this line
(setq x (strcat "East " x))
(setq y (strcat "North " y))
(setq z (strcat "Elev. " z)) ;*you may delete this line
(command "_LEADER" p textloc "" x y z "") ;*you may delete z
)
)
;
;
(defun c:en (/ p x y z ptcoord textloc)
(while
(setq p (getpoint "
Pick Point: "))
(setq textloc (getpoint "
Pick Label Location: "))
(setq x (rtos (car p)))
(setq y (rtos (cadr p)))
;(setq z (rtos (caddr p))) ;*you may delete this line
(setq x (strcat "East " x))
(setq y (strcat "North " y))
;(setq z (strcat "Elev. " z)) ;*you may delete this line
;(command "_LEADER" p textloc "" x y z "") ;*you may delete z
(command "_LEADER" p textloc "" x y "") ;*you may delete z
)
)