(setq p2 '((1 8 20) (2 4 8) (3 7 3) (4 6 23)))

(defun except (item p)
  (cond ((null p) nil)
	((eq (caar p) item)
	 (except item (cdr p)))
	(t
	 (cons (car p) (except item (cdr p))))))

(defun Nr2except (list p)
  (setq result nil)
  (while list
    (setq p (except (car list) p))
    (setq list (cdr list)))
  p)

(defun Nr2 (list p)
  (setq result nil)
  (while list
    (and (assoc (car list) p) (setq result (cons (assoc (car list) p) result)))
    (setq list (cdr list)))
; どんどんconsで突っ込んでいくから逆転
  (reverse result))

; sortは元の変数を変更するという副作用がある
; いったん別の変数に複製してからソートする
(defun edd (p) (setq pp (copy-sequence p)) (sort pp '(lambda (x y) (< (caddr x) (caddr y)))))

(defun Qr (list) (apply '+ (mapcar 'cadr list)))

;;; Σ(j∈Nr)|Lj(Ci)|
(defun sigma_LjCi (qr nr)
  (setq start qr)
  (setq s 0)
  (while nr
    (setq it (car nr))
    (setq s (+ s (abs (- (+ start (cadr it)) (caddr it)))))
    (setq start (+ start (cadr it)))
    (setq nr (cdr nr))
    )
  s
  )

(defun maxedd (l)
  (setq retval nil)
  (setq s 0)
  (while l
    (setq it (car l))
    (setq retval (cons (max 0 (- (+ s (cadr it)) (caddr it))) retval))
    (setq s (+ s (cadr it)))
    (setq l (cdr l))
    )
  (reverse retval))

;;; max((j∈^Nr))Tj(EDD)
(defun maxTjEDD (list p)
  (apply 'max (maxedd (edd (Nr2except list p))))
  )

;;; Σ(j∈^Nr)max(0, dj-Qr)
(defun sigma_max (qr p)
  (apply '+ (mapcar '(lambda (x) (max 0 (- (caddr x) qr))) p)))

(defun LB2 (list p)
  (setq nr (Nr2 list p))
  (setq notnr (Nr2except list p))
  (+ (sigma_LjCi (Qr notnr) nr)
     (maxTjEDD list notnr)
     (sigma_max (Qr notnr) notnr))
  )
