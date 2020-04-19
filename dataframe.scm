#!/usr/local/bin/guile -s
!#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; guile-dataframe
;; Author: Taylor Schmidt
;;
;; Dataframe functionality for Guile. Includes functions and utilities
;; for financial indicators.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-modules (ice-9 pretty-print))
(use-modules (ice-9 rdelim))

(use-modules (srfi srfi-1))
(use-modules (srfi srfi-9 gnu))

;;
;; Record Types
;;

;; Dataframe record type. A dataframe consists of a headers field which is an
;; alist of field names and their associated index inside each row of data. The
;; data field contains a list of lists which house the actual data.
(define-immutable-record-type <dataframe>
  (dataframe headers data)
  dataframe?
  (headers df-headers set-df-headers)
  (data df-data set-df-data))

;;
;; Conversion functions
;;

(define (parse-csv-line line-str)
  (let* ((strip-chars (char-set #\lf #\cr))
	 (parsed (string-delete strip-chars line-str)))
    (string-split parsed #\,)))

(define (parse-dataframe-headers)
  (let* ((hdr-list (parse-csv-line (read-line)))
	 (index 0))
    (map-in-order
     (lambda (h)
       (set! index (1+ index))
       `(,h . ,(1- index)))
     hdr-list)))

(define (parse-dataframe-data)
  (let loop ((line (read-line))
	     (data '()))
    (cond ((not (eof-object? line))
	   (begin
	     (loop (read-line)
		   (append data `(,(parse-csv-line line))))))
	  (#t data))))

(define (csv->dataframe file-path)
  "Converts a CSV file into a dataframe. The CSV file must begin with a header
line containing all field names. Only comma delimited CSVs are supported."
  (with-input-from-file file-path
    (lambda ()
      (let* ((headers (parse-dataframe-headers))
	     (data (parse-dataframe-data)))
	(dataframe headers data)))))

;;
;; Accessor functions
;;

(define (hdr-index headers col-name)
  (cdr (assoc col-name headers)))

(define (dataframe-hdr-index df col-name)
  "Gets the associated list index of a given column in the dataframe."
  (hdr-index (df-headers df) col-name))

(define (dataframe-column df col-name)
  "Returns a list containing all data for a given column name."
  (let ((data (df-data df))
	(index (dataframe-hdr-index df col-name)))
    (map-in-order
     (lambda (p)
       (list-ref p index))
     data)))

(define (dataframe-row df row-index)
  "Returns a list containing the row data for a given index."
  (let ((data (df-data df)))
    (list-ref data row-index)))

(define (dataframe-field df col-name row-index)
  "Returns the field value for a given column name and row index."
  (let ((data (df-data df))
	(index (dataframe-hdr-index df col-name)))
    (list-ref (list-ref data row-index) index)))

;;
;; Modification functions
;;

(define (row-apply headers data function)
  (let* ((row (last data)))
    (append row `(,(function headers data)))))

(define (dataframe-rolling-apply df new-col-name function)
  "Applies a function incrementally to a dataframe with unlimited lookback.
This is necessary for calculating things like moving averages and the like."
  (let ((headers (df-headers df))
	(data (df-data df)))
    (let loop ((index 1)
	       (new-data '()))
      (cond ((< (length new-data) (length data))
	     (loop (1+ index)
		   (append new-data `(,(row-apply headers (take data index) function)))))
	    (#t (dataframe (acons new-col-name (length headers) headers)
			   new-data))))))

(define (dataframe-clean df)
  "Cleans a dataframe by removing any fields that contain #f or empty strings."
  (dataframe (df-headers df)
	     (remove (lambda (r) (or (member #f r) (member "" r)))
		     (df-data df))))

;;
;; Basic calculations (needed to build up indicators)
;;

(define (percent-change t1 t2)
  (/ (- t2 t1) t1))

(define (gap headers df)
  (cond ((< (length df) 2) #f)
	(#t (let* ((open-index (hdr-index headers "o"))
		   (close-index (hdr-index headers "c"))
		   (compare (take-right df 2))
		   (r1 (string->number (list-ref (list-ref compare 0) close-index)))
		   (r2 (string->number (list-ref (list-ref compare 1) open-index))))
	      (- r2 r1)))))


(define apple (csv->dataframe "AAPL.csv"))
