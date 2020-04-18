#!/usr/local/bin/guile -s
!#

(use-modules (ice-9 pretty-print))
(use-modules (ice-9 rdelim))
(use-modules (srfi srfi-9 gnu))

(define-immutable-record-type <dataframe>
  (dataframe headers data)
  dataframe?
  (headers df-headers set-df-headers)
  (data df-data set-df-data))

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

(define (csv-to-dataframe file-path)
  (with-input-from-file file-path
    (lambda ()
      (let* ((headers (parse-dataframe-headers))
	     (data (parse-dataframe-data)))
	(dataframe headers data)))))

(define (dataframe-hdr-index df col-name)
  (cdr (assoc col-name (df-headers df))))

(define (dataframe-column df col-name)
  (let ((data (df-data df))
	(index (dataframe-hdr-index df col-name)))
    (map-in-order
     (lambda (p)
       (list-ref p index))
     data)))

(define (dataframe-row df row-index)
  (let ((data (df-data df)))
    (list-ref data row-index)))

(define (dataframe-field df col-name row-index)
  (let ((data (df-data df))
	(index (dataframe-hdr-index df col-name)))
    (list-ref (list-ref data row-index) index)))

(define (dataframe-apply df new-col-name function)
  (let* ((headers (df-headers df))
	 (data (df-data df))
	 (row-apply (lambda (r) (append r `(,(function r))))))
      (dataframe
       (acons new-col-name (length headers) headers)
       (map-in-order row-apply data))))

(define apple (csv-to-dataframe "AAPL.csv"))

(set! apple (dataframe-apply apple "addone" add-one))
