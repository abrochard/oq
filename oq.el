;;; package --- Summary

;;; Commentary:

;;; Code:


;;;; .[0] | .section

(defvar oq--input nil)

(defconst oq--query-buffer "*oq - query*")
(defconst oq--output-buffer "*oq - output*")

(defun oq ()
  "Open up the org query tool."
  (interactive)
  (setq oq--input (buffer-name))
  (oq--output nil)
  (pop-to-buffer oq--query-buffer)
  ;; (set-window-text-height nil 5)
  (oq-mode)
  ;; (oq--insert (oq--parser "0 0 1 0 :bullet" content))
  )

(defun oq--output (result)
  "RESULT."
  (pop-to-buffer oq--output-buffer)
  (view-mode-disable)
  (erase-buffer)
  (when result
    (oq--insert result)
    (pp-buffer))
  (emacs-lisp-mode)
  (view-mode))

(define-minor-mode oq-mode
  "oq minor mode."
  :init-value nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") 'oq-apply)
            (define-key map (kbd "C-c C-d") 'oq-build-query)
            map))

(defun oq-apply ()
  "Something."
  (interactive)
  (let ((query (buffer-string)))
    (pop-to-buffer oq--input)
    (oq--output (oq--parser query (org-element-parse-buffer)))
    (pop-to-buffer oq--query-buffer)))

(defun oq-build-query ()
  "Something."
  (interactive)
  (let ((query (buffer-string)))
    (message "%s" (oq--builder (split-string query " ")))))

(defun oq--parser (str content)
  "STR CONTENT."
  (let ((output content))
    (dolist (cmd (split-string str " "))
      (setq output
            (cond ((string-match "\\([0-9]+\\)" cmd) (oq--child output (string-to-number (match-string-no-properties 1 cmd))))
                  ((string-match "\\(:[^ ]+\\)" cmd) (oq--prop output (car (read-from-string (match-string-no-properties 1 cmd)))))
                  ((string-match "\\('[^ ]+\\)" cmd) (oq--map output (car (read-from-string (match-string-no-properties 1 cmd)))))
                  (t output)))
      )
    output))

(defun oq--builder (cmd-list)
  "CMD-LIST."
  (let ((cmd (car cmd-list))
        (rest (cdr cmd-list)))
    (format "(%s%s)"
            (cond ((string-match "\\([0-9]+\\)" cmd) (format "(nth %s (org-element-contents " (match-string-no-properties 1 cmd)))
                  ((string-match "\\(:[^ ]+\\)" cmd) (format "(org-element-property %s " (match-string-no-properties 1 cmd)))
                  ((string-match "\\('[^ ]+\\)" cmd) (format "(org-element-map el %s #'identity)" (match-string-no-properties 1 cmd)))
                  (t ""))
            (if rest
                (concat " " (oq--builder rest)) "(org-element-parse-buffer)"))))

(defun oq--down (el seq)
  "EL SEQ."
  (let ((c el))
    (dolist (n seq)
      (setq c (oq--child c n))
      )
    c))

(defun oq--child (el n)
  "EL N."
  (nth n (org-element-contents el)))

(defun oq--prop (el prop)
  "EL PROP."
  (if (org-element-type el)
      (org-element-property prop el)
    (mapcar (lambda (x) (oq--prop x prop)) el)))

(defun oq--map (el type)
  "EL TYPE."
  (org-element-map el type #'identity))

(defun oq--insert (el)
  "EL."
  (unless (not el)
    (insert (format "%s" (oq--remove-parent el)))))

(defun oq--remove-parent (el)
  "EL."
  (cond ((org-element-type el) (org-element-put-property el :parent nil))
        ((listp el) (mapcar #'oq--remove-parent el))
        (t el)))

(provide 'oq)
;;; oq.el ends here
