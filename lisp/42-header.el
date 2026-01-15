;;; 42-header.el --- 42 Header-*- lexical-binding: t; -*-

;;; Commentary:
;; just do it
;;;
;;; Code:
(defvar asciiart
  '("         :::      ::::::"
    "       :+:      :+:    :+:"
    "     +:+ +:+           +:+  "
    "   +#+  +:+         +#+     "
    "  +#+#+#+#+#+      +#+       "
    "       #+#      #+#         "
    "      ###      ########.fr   "))

(defvar stdheader-start "/*")
(defvar stdheader-end "*/")
(defvar stdheader-fill "*")
(defvar length 80)
(defvar margin 5)

(defvar types
  '(("\\.\\(c\\|h\\|cc\\|hh\\|cpp\\|hpp\\|tpp\\|ipp\\|cxx\\|go\\|rs\\|php\\|py\\|java\\|kt\\|kts\\)$" . ("/*" "*/" "*"))
    ("\\.\\(htm\\|html\\|xml\\)$" . ("" "*"))
    ("\\.\\(js\\|ts\\)$" . ("//" "//" "*"))
    ("\\.tex$" . ("%" "%" "*"))
    ("\\.\\(ml\\|mli\\|mll\\|mly\\)$" . ("(*" "*)" "*"))
    ("\\.\\(vim\\|vimrc\\)$" . ("\"" "\"" "*"))
    ("\\.\\(el\\|emacs\\|asm\\)$" . (";" ";" "*"))
    ("\\.\\(f90\\|f95\\|f03\\|f\\|for\\)$" . ("!" "!" "/"))
    ("\\.lua$" . ("--" "--" "-"))))

(defun filetype ()
  "Check the current file type."
  (let ((f (filename)))
    (setq stdheader-start "#")
    (setq stdheader-end "#")
    (setq stdheader-fill "*")
    (dolist (type types)
      (when (string-match (car type) f)
        (setq stdheader-start (cadr type))
        (setq stdheader-end (caddr type))
        (setq stdheader-fill (cadddr type))))))

(defun ascii (n)
  (nth (- n 3) asciiart))

(defun textline (left right)
  (let* ((lleft (substring left 0 (min (length left) (- length (* margin 2) (length right)))))
         (padding (- length (* margin 2) (length lleft) (length right))))
    (concat stdheader-start (make-string (- margin (length stdheader-start)) ? ) lleft (make-string padding ? ) right (make-string (- margin (length stdheader-end)) ? ) stdheader-end)))

(defun line-func (n)
  (cond
   ((or (= n 1) (= n 11)) (concat stdheader-start " " (make-string (- length (length stdheader-start) (length stdheader-end) 2) (string-to-char stdheader-fill)) " " stdheader-end))
   ((or (= n 2) (= n 10)) (textline "" ""))
   ((member n '(3 5 7)) (textline "" (ascii n)))
   ((= n 4) (textline (filename) (ascii n)))
   ((= n 6) (textline (concat "By: " (user) " <" (mail) ">") (ascii n)))
   ((= n 8) (textline (concat "Created: " (date) " by " (user)) (ascii n)))
   ((= n 9) (textline (concat "Updated: " (date) " by " (user)) (ascii n)))))

(defun user ()
  (or (and (boundp 'user42) user42)
      (let ((u (getenv "USER")))
        (if (> (length u) 0) u "marvin"))))

(defun mail ()
  (or (and (boundp 'mail42) mail42)
      (let ((m (getenv "MAIL")))
        (if (> (length m) 0) m "marvin@42.fr"))))

(defun filename ()
  (let ((fn (if (buffer-file-name) (file-name-nondirectory (buffer-file-name)) "< new >")))
    fn))

(defun date ()
  (format-time-string "%Y/%m/%d %H:%M:%S"))

(defun insert-header ()
  (save-excursion
    (goto-char (point-min))
    (let ((header ""))
      (dotimes (i 11)
        (setq header (concat (line-func (- 11 i)) "\n" header)))
      (insert (concat header "\n")))))

(defun update ()
  (filetype)
  (save-excursion
    (goto-char (point-min))
    (if (and (>= (line-number-at-pos (point-max)) 9)
             (progn (forward-line 8)
                    (string-match (concat "^" (regexp-quote stdheader-start) (make-string (- margin (length stdheader-start)) ? ) "Updated: ") (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
        (progn
          (when (buffer-modified-p)
            (delete-region (line-beginning-position) (line-end-position))
            (insert (line-func 9)))
          (goto-char (point-min))
          (forward-line 3)
          (delete-region (line-beginning-position) (line-end-position))
          (insert (line-func 4))
          nil)
      t)))

(defun stdheader ()
  (interactive)
  (when (update)
    (insert-header)))

(add-hook 'before-save-hook (lambda () (update)))

(provide '42-header)
;;; 42-header.el ends here
