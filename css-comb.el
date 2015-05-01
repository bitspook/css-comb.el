;;; Commentary
;;; css-comb.el --- Sort CSS properties in a particular order using Css Comb (https://github.com/csscomb/csscomb.js)

;; Copyright (C) 2015 Charanjit Singh <ckhabra@gmail.com>

;; Author: Charanjit Singh <ckhabra@gmail.com>
;; Version: 0.1
;; URL: https://github.com/channikhabra/css-comb-el

;;; Code:

(defvar css-comb-executable "echo"
  "Executable to use for combing the CSS")

(defconst css-comb-args '())

(defun css-comb-command-not-found-message (program)
  "Construct a message about PROGRAM not found."
  (format
   "%s not found. Install it with: \"npm -g install csscomb\" "
   program))

(defun css-comb-format-error-message (bufname)
  "Construct a format error message with BUFNAME."
  (format
   "Could not apply csscomb. See %s to check errors for details"
   bufname))

(defun css-comb-format-region (program beg end)
  "By PROGRAM, format each line in the BEG .. END region."
  (if (executable-find program)
      (save-excursion
        (apply 'call-process-region beg end program t
               (list t nil) t css-comb-args))
    (message (css-comb-command-not-found-message program))))

(defun css-comb-format-buffer (program extenstion)
  "By PROGRAM, format current buffer with EXTENSTION."
  (if (executable-find program)
      (css-comb-format-buffer-1 program extenstion)
    (message (css-comb-command-not-found-message program))))


(defun css-comb-format-buffer-1 (program extenstion)
  "Internal function of `css-comb-format-buffer'.

By PROGRAM, format current buffer with EXTENSTION."
  (let* ((tmpfile (make-temp-file "css-comb" nil
                                  (format ".%s" extenstion)))
         (outputbufname (format "*css-comb-%s*" extenstion))
         (outputbuf (get-buffer-create outputbufname))
         (args (append css-comb-args (list tmpfile))))
    (unwind-protect
        (progn
          (with-current-buffer outputbuf (erase-buffer))
          (write-region nil nil tmpfile)

          (if (zerop (apply 'call-process program nil nil nil args))
              (progn
                (with-current-buffer outputbuf
                  (insert-file-contents tmpfile))
                (let ((p (point)))
                  (save-excursion
                    (with-current-buffer (current-buffer)
                      (erase-buffer)
                      (insert-buffer-substring outputbuf)))
                  (goto-char p)
                  (message "Applied css-comb")
                  (kill-buffer outputbuf)))
            (message (css-comb-format-error-message outputbufname))
            (display-buffer outputbuf)))
      (progn
        (delete-file tmpfile)))))

;;;###autoload
(defun css-comb ()
  "Comb region if active, otherwise the current buffer.

Formatting is done according to the csscomb command."
  (interactive)
  (if (use-region-p)
      (css-comb-format-region
       css-comb-executable
       (region-beginning) (region-end))
    (css-comb-format-buffer css-comb-executable "css")))


(provide 'css-comb)

;; Local Variables:
;; coding: utf-8
;; eval: (checkdoc-minor-mode 1)
;;; css-comb.el ends here
