;;; delve-calender.el --- View buffers by time -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Natnael Kahssay
;;
;; Author: Natnael Kahssay <https://github.com/natask>
;; Maintainer: Natnael Kahssay <thisnkk@gmail.com>
;; Created: September 29, 2021
;; Modified: September 29, 2021
;; Version: 0.0.1
;; Keywords: delve org-roam filter
;; Homepage: https://github.com/natask/delve-calender
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  View buffers by time
;;
;;; Requirements:
(require 'ts)
(require 'delve)
(require 'cl-lib)

;;; Code:
(defun  delve-calender-cartisan-product (fun a b)
  (reduce 'append
          (mapcar
           (lambda (x)
             (mapcar
              (lambda (y)
                (funcall fun x y))
              b))
           a)))

(defun delve-calender--filter-time-fn (time type item)
  (if (delve--zettel-p item)
      (let ((ts-item
             (->> (cl-struct-slot-value 'org-roam-node type (delve--zettel-node item))
                  (float-time)
                  (make-ts :unix))))
        (ts< ts-item time))
    'nil))

(defun delve-calender--filter-fn (&rest args)
  (declare (advertised-calling-convention (&rest slots type item) nil))
  (-let* ((slots (butlast args 2))
          (type (car (last args 2)))
          (item (-last-item args)))
    (if (delve--zettel-p item)
        (let ((ts-item
               (->> (cl-struct-slot-value 'org-roam-node type (delve--zettel-node item))
                    (float-time)
                    (make-ts :unix))))
          (ts< ts-item (apply #'delve-calender-ts slots)))
      'nil)))

(cl-defun delve-calender-create-filter-time-functions (&optional (types '((file-mtime) (file-atime) (ctime))))
  "Create functions with prefix and filters derived from TYPES."
  (dolist (type types)
    (let* ((closure `(lambda (buf)
                       ,(format "Filter by %s after selected time in BUF." type)
                       (interactive (list (current-buffer)))
                       (let ((selected-time (make-ts :unix (float-time (org-read-date '(16) 't)))))
                         (lister-set-filter (lister-get-ewoc buf) (apply-partially 'delve-calender--filter-time-fn  selected-time ',(car type)))))))
      (fset (intern (concat "delve-calender-filter-select-by" "-" (prin1-to-string (car type)))) closure))))

(cl-defun delve-calender-create-filter-functions (&optional (slots-lt '((:month 0) (:week 0) (:day 0)))
                                                            (types  '((mtime) (atime) (ctime))))
  "Create functions with prefix and filters derived from cartisan product of SLOTS-LT and TYPES."
  (let ((fn-metas (delve-calender-cartisan-product (lambda (slots type)
                                                     "Get function name postfix and function args from SLOTS and TYPE."
                                                     (cons
                                                      (reduce (lambda (x y) (concat x "-" y)) (append (mapcar #'prin1-to-string slots) (mapcar #'symbol-name type)))
                                                      (append slots type)))
                                                   slots-lt types)))
    (dolist (fn-meta fn-metas)
      (let* ((slots (butlast (cdr fn-meta)))
             (type  (-last-item (cdr fn-meta)))
             (closure `(lambda (buf)
                         ,(format "Filter by %s based on slots %s in BUF." (last (cdr fn-meta)) (butlast (cdr fn-meta)))
                         (interactive (list (current-buffer)))
                         (lister-set-filter (lister-get-ewoc buf) (apply-partially 'delve-calender--filter-fn ,@slots ',type)))))
        (fset (intern (concat "delve-calender-filter-by" (car fn-meta))) closure)))))

(cl-defun delve-calender-ts (&key (type 'strict) day week month year)
  "strict verses rolling."
  (--> (ts-now)
       (if day
           (when (eq type 'strict) (ts-apply :hour 0 :minute 0 :second 0 it))
         (setq day 0)
         it)
       (if week
           (when (eq type 'strict)
             (ts-apply :hour 0 :minute 0 :second 0 it)
             (ts-adjust 'day (- (ts-dow it)) it))
         (setq week 0)
         it)
       (if month
           (when (eq type 'strict) (ts-apply :hour 0 :minute 0 :second 0 :day 0 it))
         (setq month 0)
         it)
       (if year
           (when (eq type 'strict) (ts-apply :hour 0 :minute 0 :second 0 :day 0 :month 0 it))
         (setq year 0)
         it)
       (ts-adjust 'day day 'day (* 7 week) 'month month 'year year it)))

(delve-calender-create-filter-functions)
(delve-calender-create-filter-time-functions)

(provide 'delve-calender)
;;; delve-calender.el ends here
