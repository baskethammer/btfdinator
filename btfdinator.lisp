;;;; btfdinator.lisp

(in-package #:btfdinator)

(defparameter *db* nil)

(defun peaks (series)
  (iter (for i from 1 to (- (length series) 2))
    (let ((this     (elt series i))
	  (previous (elt series (- i 1)))
	  (next     (elt series (+ i 1))))
      (if (and
	   (> this previous)
	   (> this next))
	  (collect i)))))

(defun gain-pct (peak-price start-price)
  (/ (- peak-price start-price) start-price))

(defun peak-volume (instrument peak)
  (let* ((closes (instrument-closes instrument))
	 (peak-close (elt closes peak))
	 (prev-close (elt closes (- peak 1))))
    (if (>= peak-close prev-close)
	(elt (instrument-volumes instrument) peak)
	(elt (instrument-volumes instrument) (- peak 1)))))

(defun cycle (instrument peak)
  (let* ((highs       (instrument-highs instrument))
	 (lows        (instrument-lows instrument))
	 (peak-price  (elt highs peak)))
    (iter (for i from (- peak 1) downto (max 0 (- peak 8)))
      (for days upfrom 1)
      (let* ((this-low (elt lows i))
	     (gain     (gain-pct peak-price this-low)))
	(if (>= gain 0.20)
	    (leave (list days
			 gain)))))))

(defun cycles (instrument &optional (cycle-max 5))
  (let* ((rpeaks (nreverse (peaks (instrument-highs instrument))))
	 (cycle-count 0)
	 (next-peak-max 1000))
    (iter (for rpeak in rpeaks)
      (if (and (< cycle-count cycle-max)
	       (< rpeak next-peak-max))
	  (let* ((cycle-result (cycle instrument rpeak))
		 (cycle-days   (first cycle-result)))
	    (if cycle-result
		(progn
		 (setf next-peak-max
		       (- rpeak cycle-days))
		 (incf cycle-count)
		 (collect (push rpeak cycle-result)))
		()))))))

(defun average (number-list)
  (let ((denominator (length number-list))
	(numerator (apply #'+ number-list)))
    (/ numerator denominator)))

      
(defun rank (cycles &optional (min-cycles 5))
  (if (>= (length cycles) min-cycles)
      (let ((avg-gain (average (mapcar #'(lambda (x) (elt x 2)) cycles)))
	    (avg-days (average (mapcar #'(lambda (x) (elt x 1)) cycles))))
	(* 100
	   (/ avg-gain avg-days)))))

(defun rank-instrument (instrument)
  (rank (cycles instrument)))

(defun ranks (instruments)
  (iter (for i in instruments)
    (generate j upfrom 0)
    (let ((this-rank (rank-instrument i)))
      (if this-rank
	  (collect (list (next j) this-rank))
	  (next j)))))

(defun du-month (volumes month-ago)
  (let* ((offset (* month-ago 20))
	 (volume-count (- (length volumes) 1))
	 (start (- volume-count (* (- month-ago 1) 20)))
	 (end  (- start 20))
	 (du (elt volumes start)))
    (iter (for i from (- start 1) downto end)
      (let ((this-volume (elt volumes i)))
	(if (< this-volume du)
	    (setf du this-volume))))
    du))
    
(defun du (instrument)
  (let ((volumes (instrument-volumes instrument)))
    (if (> (length volumes) 62)
	(let* ((m1 (du-month volumes 1))
	       (m2 (du-month volumes 2))
	       (m3 (du-month volumes 3)))
	  (/ (+ m1 m2 m3) 3)))))
	

(defun last-volume (instrument)
  (let* ((volumes (instrument-volumes instrument))
	 (lastndx (- (length volumes) 1)))
    (elt volumes lastndx)))
    
  
(defun du-p (instrument)
  (let ((avgdu (du instrument)))
    (if avgdu
	(let* ((avgdu (du instrument))
	       (highdu (* 1.4986 avgdu))
	       (volume (last-volume instrument)))
	  (< volume highdu)))))
	   
(defun watchlist-ndxs (ranks instruments)
  (let ((instrument-ndxs (mapcar #'first ranks)))
    (iter (for i in instrument-ndxs)
      (if (du-p (elt instruments i))
	  (collect i)))))

(defun watchlist-ndxs-filters (ranks instruments functions)
  (let* ((instrument-ndxs (mapcar #'first ranks)))	
    (iter (for f in functions)
      (setf instrument-ndxs
	    (iter (for i in instrument-ndxs)
	      (let ((instrument (elt instruments i)))
		(if (funcall f instrument)
		    (collect i))))))
    instrument-ndxs))
      

(defun watchlist-tickers (ranks instruments)
  (let ((instrument-ndxs (mapcar #'first ranks)))
    (iter (for i in instrument-ndxs)
      (let ((instrument (elt instruments i)))
	(if (du-p instrument)
	    (collect (instrument-ticker instrument)))))))
  


(defun tickers-from-ndxs (ndxs instruments)
  (mapcar #'(lambda (x)
	      (instrument-ticker (elt instruments x)))
	  ndxs))

(defun sma-uptrend-p-instrument (instrument)
  (let ((series (instrument-closes instrument)))
    (sma-uptrend-p series 20 50)))

(defun watchlist-filters (ranks instruments functions)
  (let* ((instrument-ndxs (mapcar #'first ranks)))	
    (iter (for f in functions)
      (setf instrument-ndxs
	    (iter (for i in instrument-ndxs)
	      (let ((instrument (elt instruments i)))
		(if (funcall f instrument)
		    (collect i))))))
    (list instrument-ndxs (tickers-from-ndxs instrument-ndxs instruments))))

(defun last-close (instrument)
  (let* ((closes (instrument-closes instrument))
	 (c-len  (length closes)))
    (elt closes (- c-len 1))))
	 

(defun print-watchlist (ndxs instruments)
  (iter (for i in ndxs)
    (let* ((instrument (elt instruments i))
	   (ticker (instrument-ticker instrument))
	   (i-du (du instrument))
	   (low-band (* (- 1 0.4986) i-du))
	   (frv (* 3 i-du))
	   (lclose (last-close instrument)))
      (collect `(,ticker ,low-band ,frv, lclose)))))


(defun run-btfd-list (sql-query-path db-path)
  (with-open-database (*yeodl-db* db-path)
  (let* ((tickers (get-tickers-from-file sql-query-path))
	 (instruments (get-instruments-from-tickers tickers
						    (get-date-bars-ago 127)))
	 (rank-and-ndx (ranks instruments)))
    (setf rank-and-ndx (sort rank-and-ndx #'> :key #'second))
    (print-watchlist
     (watchlist-ndxs-filters rank-and-ndx instruments '(du-p sma-uptrend-p-instrument))
     instruments))))
