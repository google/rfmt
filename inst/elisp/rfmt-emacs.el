;;; rfmt_emacs.el --- Extremely crude integration of Emacs and rfmt

;; Keywords: tools, R
;; Package-Requires: ((cl-lib "0.3"))

;; To use, ensure the directory of this file is in your `load-path' and add:
;;
;;   (setq rfmt-executable "-- path to rfmt executable --")
;;   (require 'rfmt_emacs)
;;
;; to your .emacs configuration.

;; You may also want to bind 'rfmt-buffer to a key, e.g.:
;;
;;   (global-set-key (kbd "C-i") 'rfmt-buffer)
;;
;; When invoked, 'rfmt-buffer formats the entire contents of the current buffer
;; (which must constitute syntactically-correct R) using rfmt.

(defun rfmt-buffer ()
  "Format the current buffer using rfmt."
  (interactive)
  (let ((cur-buf (current-buffer))
        (cur-text (buffer-string)))
    (with-temp-buffer
      (insert cur-text)
      (let (status)
        (setq status
          (call-process-region (point-min) (point-max) rfmt-executable t t))
  (if (equal 0 status)
      (buffer-swap-text cur-buf)
      (error "rfmt error: %s" (buffer-string)))))))

(provide 'rfmt-emacs)
