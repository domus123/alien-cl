(ql:quickload :cl-ppcre) ;;split

(defparameter *csv-separator* #\,)

(defclass cl-series ()
  (( series-size
     :initarg :size
     :initform 0)
   ( header
     :initarg :header
     :initform nil)
   series ))

(defmethod initialize-instance :after ((serie cl-series) &key lst )
  (let ( (size (slot-value serie 'series-size)))
    "Getting all the information needed to create the array"
    (setf (slot-value serie 'series)
	  (make-array size :initial-contents lst))))

(defmethod get-array ((serie cl-series))
  (slot-value serie 'series))



;; ------------------------------MACROS-------------------------------------------------------
;;Only for not having to write many loops for all the print methos

(defmacro head-print-macro (&body body)
  `(let* ( (header (slot-value serie 'header))
	  (size (slot-value serie 'series-size)) ;;'(line column)
	  (line (car size))
	  (column (cadr size))
	  (array (slot-value serie 'series)))
     ,@body))


(defmacro header-macro ()
  `(if header (progn
		(format t "~&|")
		(loop for j below column
		      do
		      (format t "~10@a" (aref array 0 j)))
		(format t "|~&|")
		(loop for j below column
		      do
		      (format t "~10@a" "------"))
		(format t "|"))
     nil))

(defmacro loop-macro (n)
  `(loop for i from (if header 1 0) below ,n ;;Ugly as fuck line
	  do
	  (progn
	    (format t "~&|") ;;breakline at each new line
	    (loop for j below column
		do
		(format t "~10@a" (aref array i j)))
	    (format t "|"))))

;;------------------------------------- END OF MACROS ----------------------------------------

(defmethod p-table ((serie cl-series))
  (head-print-macro (header-macro) (loop-macro line)))

(defmethod p-head ((serie cl-series))
  (head-print-macro (header-macro) (loop-macro 6)))

(defun get-info (lst )
  (let* ( (column-number (length (car lst)))
	  (line-number (length lst))
	  (size-lst (list line-number column-number)))
    size-lst))

(defun csv-open (path-name)
  (with-open-file (stream path-name
			  :direction :input
			  :external-format :iso-8859-1)
		  (loop for line = (read-line stream nil :eof)
			until (equal line :eof)
			collect (cl-ppcre:split *csv-separator* line)) )
  nil)

(defun read-from-csv (file-name &key header )
  (let* ( (list (csv-open file-name))
	  (size-lst (get-info list)))
    (make-instance 'cl-series :size size-lst :header header :lst list)))


;;(let ( (alien (read-from-csv "milhao.csv" :header t)))
  ;;(p-head alien))
       
  
  

  
