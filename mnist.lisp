(ql:quickload :fare-csv)

					;(setf *print-length* nil)

					; load data

					;functions


(defparameter *train-data*  (fare-csv:with-rfc4180-csv-syntax ()
		   (let ((fare-csv:*separator* #\,))
		     (fare-csv:read-csv-file "mnist_train.csv"))))

(defparameter *test-data*  (fare-csv:with-rfc4180-csv-syntax ()
			      (let ((fare-csv:*separator* #\,))
				(fare-csv:read-csv-file "mnist_validation.csv"))))

(defparameter *integer-train-data* (parse-nested-list *train-data*))

(defparameter *integer-test-data* (parse-nested-list *test-data*))

(defparameter *train-target* (get-target *integer-train-data*))

(defparameter *test-target* (get-target *integer-test-data*))

(defparameter *un-normalized-train-data* (get-data *integer-train-data*))

(defparameter *un-normalized-test-data* (get-data *integer-test-data*))

(defparameter *train-normalized* (normalize-nested-list *un-normalized-train-data*))

(defparameter *test-normalized* (normalize-nested-list *un-normalized-test-data*))

(defparameter *train-data* (add-bias *train-normalized*))

(defparameter *test-data* (add-bias *test-normalized*))

(defparameter *epoch* 50)
(defparameter *eta* 0.1)
(defparameter *hidden-inputs* 100)

(defparameter test '( (1 2 3 4) (3 2 3 4) (3 4 6)))

(defun parse (row)
  (if (null (car row)) nil
      (cons (parse-integer (car row))
	    (parse (cdr row)))))

(defun parse-nested-list (multi-dimensional-list)
  (if (null (car multi-dimensional-list)) nil
      (cons (parse (car multi-dimensional-list))
	    (parse-nested-list (cdr  multi-dimensional-list)))))

(defun get-target (data)
  (if (null (car data)) nil
      (cons (caar data)
	    (get-target (cdr data)))))

(defun get-data (data)
  (if (null (car data)) nil
      (cons (cdar data)
	    (get-data (cdr data)))))

(defun dot (a b)
  (reduce #'+ (map 'simple-vector #'* a b)))

(defun normalize (row)
  (if (null (car row)) nil
      (cons (/ (car row) 255.0)
	    (normalize (cdr row)))))

(defun normalize-nested-list (multi-dimensional-list)
  (if (null (car multi-dimensional-list)) nil
      (cons (normalize (car multi-dimensional-list))
	    (normalize-nested-list (cdr multi-dimensional-list)))))

(defun add-bias (data)
  (if (null (car data)) nil
      (cons (append '(1) (car data))
	    (add-bias (cdr data)))))

(defun weight ()
  (if (eq 0 (random 2))
      (random 0.5)
      (- (random 0.5))))

(defun weight-inner (hidden-inputs train-data)
  (let (rows) (length (car train-data))
    (loop for x from 1 to rows
	  collect (loop for y from 1 to hidden-inputs
			collect (weight)))))

(defparameter *weight-inner* (weight-inner *hidden-inputs* *train-data*))
