;;; indicators.lisp

(in-package #:btfdinator)

(defun sma (series period)
  (let* ((len (length series))
	 (start (- len period)))
     (iter (for i from start to (- len 1))
       (sum (aref series i) into x)
       (declare (double-float x))
       (finally (return (/ x period))))))
	 

(defun sma-uptrend-p (series fast slow)
  (let ((fastma (sma series fast))
	(slowma (sma series slow)))
    (if (> fastma slowma)
	t)))
