;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((org-src-preserve-indentation . t)
     (eval let
      ((year-dir (locate-dominating-file default-directory "Cargo.toml")))
      (when
          (and year-dir (not (string= year-dir (projectile-project-root)))
               (string-match "/\\([0-9]\\{4\\}\\)/$" year-dir))
        (setq-local projectile-project-root year-dir)
        (setq-local compile-command (format "cd %s && cargo run" year-dir))
        (setq-local default-directory year-dir)))
     (eval local-set-key (kbd "C-c c f") 'format-current-file)
     (eval local-set-key (kbd "C-c c v") 'norm-check-current-dir)
     (eval local-set-key (kbd "C-c c r") 'cc-compile-current-file)
     (eval local-set-key (kbd "C-c c c") 'norm-check-current-file)
     (eval progn
      (defun norm-check-current-file nil
        "Run norminette on the current file in its directory." (interactive)
        (when (buffer-file-name)
          (let ((filename (file-name-nondirectory (buffer-file-name))))
            (compile
             (concat "norminette -R CheckForbiddenSourceHeader " filename)))))
      (defun cc-compile-current-file nil
        "Run norminette on the current file in its directory." (interactive)
        (when (buffer-file-name)
          (let ((filename (file-name-nondirectory (buffer-file-name))))
            (compile (concat "cc -Werror -Wall -Wextra -I. *.c && ./a.out")))))
      (defun norm-check-current-dir nil
        "Run norminette on all files in the current project's root directory."
        (interactive)
        (let ((project-root (project-root (project-current))))
          (if project-root
              (let ((default-directory project-root))
                (compile "norminette -R CheckForbiddenSourceHeader ."))
            (message "Could not find project root."))))
      (defun format-current-file nil
        "Run c_formatter_42 on the current file and revert the buffer."
        (interactive)
        (when (buffer-file-name)
          (let ((filename (file-name-nondirectory (buffer-file-name))))
            (shell-command (concat "c_formatter_42 " filename))
            (revert-buffer t t t)
            (message "Formatted %s with c_formatter_42" filename)))))))
 '(zoom-window-mode-line-color "Black"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
