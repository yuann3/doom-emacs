(use-package! acp
  :defer t)

(use-package! agent-shell
  :defer t
  :commands (agent-shell
             agent-shell-anthropic-start-claude-code))
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; --------------------------------------------------
;; Basic identity & fonts
;; --------------------------------------------------
(setq user-full-name "Yiyuan Li"
      user-mail-address "yy@eyuan.me")

(setq doom-font         (font-spec :family "Berkeley Mono" :size 11)
      doom-unicode-font (font-spec :family "JetBrainsMono Nerd Font" :size 11 ))

;; (after! persp-mode
;;   (defun display-workspaces-in-minibuffer ()
;;     (with-current-buffer " *Minibuf-0*"
;;       (erase-buffer)
;;       (insert (+workspace--tabline))))
;;   (run-with-idle-timer 1 t #'display-workspaces-in-minibuffer)
;;   (+workspace/display))

;; --------------------------------------------------
;; Visual defaults
;; --------------------------------------------------
(setq doom-theme 'doom-tomorrow-night-hc)
;; (setq catppuccin-flavor 'mocha) ;; 'latte, 'macchiato, or 'mocha
;; (setq display-line-numbers-type 'relative)
(setq org-directory "~/Documents/Notes/org")
(setq nerd-icons-color-icons nil)

;; --------------------------------------------------
;; Modeline tweaks
;; --------------------------------------------------
(after! doom-modeline
  (setq doom-modeline-battery                  t
        lsp-modeline-code-actions-enable       nil
        doom-modeline-vcs                      t
        doom-modeline-vcs-max-length           20
        doom-modeline-modal-icon               nil
        doom-modeline-lsp                      nil
        doom-modeline-lsp-perform-update-on-save nil
        doom-modeline-workspace-name           t
        doom-modeline-bar-width                3))

(with-eval-after-load 'lsp-mode
  (setq lsp-modeline-diagnostics-enable nil
        lsp-modeline-workspace-status-enable nil))


;; --------------------------------------------------
;; Git helper
;; --------------------------------------------------
(defun my/magit-quick-commit-push ()
  "Stage, commit (autocommit msg) and push everything via Magit."
  (interactive)
  (let ((default-directory (or (projectile-project-root) (doom-project-root)))
        (commit-msg "autocommit"))
    (magit-stage-modified t)
    (magit-commit-create `("-m" ,commit-msg))
    (magit-push-current-to-pushremote nil)))

;; --------------------------------------------------
;; Script helpers
;; --------------------------------------------------
(defun my/vterm-run-cmd (cmd)
  "In vterm, run CMD from (or switch to) *vterm* buffer."
  (interactive "sCommand: ")
  (let ((buf (get-buffer-create "*vterm*")))
    (pop-to-buffer buf)
    (unless (eq major-mode 'vterm-mode) (vterm-mode))
    (vterm-send-string cmd)
    (vterm-send-return)))

(defun my/run-script ()
  "Run ./run inside project root via vterm."
  (interactive)
  (let ((default-directory (or (projectile-project-root) (doom-project-root) default-directory)))
    (my/vterm-run-cmd "./run")))

(defun my/run-pew-native ()
  "Run `pew -d .` asynchronously from project root."
  (interactive)
  (let ((default-directory (or (projectile-project-root) (doom-project-root) default-directory)))
    (async-shell-command "pew -d .")))

;; --------------------------------------------------
;; Leader keybindings
;; --------------------------------------------------
(map! :leader
      ;; Git
      (:prefix ("g" . "git")
       :desc "Quick commit & push" "p" #'my/magit-quick-commit-push)
      ;; Scripts
      :desc "Run pew -d ." "l l" #'my/run-pew-native)

(setq-default truncate-lines nil)
(global-visual-line-mode 1)

;; --------------------------------------------------
;; Silence LSP visual noise
;; --------------------------------------------------
(after! lsp-mode
  (setq lsp-ui-sideline-enable            nil
        lsp-ui-doc-enable                 nil
        lsp-lens-enable                   nil
        lsp-headerline-breadcrumb-enable  nil))
(after! lsp-rust
  (setq lsp-rust-analyzer-diagnostics-enable nil))

;; --------------------------------------------------
;; Cursor shapes (all box)
;; --------------------------------------------------
(setq evil-default-cursor           'box
      evil-normal-state-cursor      'box
      evil-insert-state-cursor      'box
      evil-visual-state-cursor      'box
      evil-replace-state-cursor     'box
      evil-emacs-state-cursor       'box)
(remove-hook 'diff-hl-mode-hook #'+vc-gutter-fix-diff-hl-faces-h)

;; --------------------------------------------------
;; Dirvish
;; --------------------------------------------------
(use-package! dirvish
  :after dired
  :config
  ;; replace all the old Dired shit with Dirvish
  (dirvish-override-dired-mode)

  (map! :leader
        :desc "Dirvish" "f D" #'dirvish)

  ;; Optional tweaks—you can hate or love these:
  (setq dirvish-mode-line-format '(:left (sort file-time file-size symlink) :right (omit yank index)))
  (setq dirvish-header-line-format '(:left (path)))
  (setq dirvish-use-header-line t))


;; --------------------------------------------------
;; flycheck | LSP
;; --------------------------------------------------
(after! flycheck
  (map! :leader
        :desc "List Flycheck errors"
        "l k" #'flycheck-list-errors))

(after! lsp-mode
  (map! :leader
        :desc "Toggle LSP diagnostics"
        "l t" #'lsp-diagnostics-mode))

;; Turn off LSP-ui docs and sideline hints
;; Completely disable LSP hover/signature popups
(after! lsp-mode
  ;; Turn off signature help (the popup when you’re inside Vec::new() or Vec<i32>)
  (setq lsp-signature-auto-activate nil
        lsp-signature-render-documentation nil)

  ;; Disable eldoc (minibuffer or echo-area docs)
  (setq lsp-eldoc-enable-hover nil)
  (remove-hook 'lsp-mode-hook #'lsp-enable-eldoc))

(after! lsp-ui
  ;; Just in case, obliterate any remaining LSP-ui popups
  (setq lsp-ui-doc-enable nil
        lsp-ui-sideline-enable nil
        lsp-ui-doc-show-with-cursor nil
        lsp-ui-doc-show-with-mouse nil))

;; --------------------------------------------------
;; deft / org
;; --------------------------------------------------
(setq deft-extensions '("txt" "tex" "org"))
(setq deft-directory "~/Documents/Notes")
(setq deft-recursive t)

(after! org
  (setq org-agenda-files '("~/Documents/Notes/org/agenda.org")))

;; auto latex render
(add-hook 'after-save-hook
          (lambda ()
            (when (eq major-mode 'org-mode)
              (org-latex-preview))))



;; --------------------------------------------------
;; macos
;; --------------------------------------------------
(when (eq system-type 'darwin)
  (dolist (ev '([C-mouse-4] [C-mouse-5] [C-mouse-6] [C-mouse-7]))
    (define-key input-decode-map ev [ignore])))

;; --------------------------------------------------
;; Web mode (i hate web but i have to do it cuz this is my job)
;; --------------------------------------------------
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.cshtml?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.svelte?\\'" . web-mode))
(setq web-mode-engines-alist
      '(("razor"    . "\\.cshtml\\'")
        ("blade"  . "\\.blade\\.")
        ("svelte" . "\\.svelte\\.")
        ))


;; --------------------------------------------------
;; Zoom the split
;; --------------------------------------------------
(use-package! zoom-window
  :commands (zoom-window-zoom)
  :bind
  (:map evil-window-map
        ("m" . zoom-window-zoom)))
(custom-set-variables
 '(zoom-window-mode-line-color "Black"))


;; --------------------------------------------------
;; Debug - Dap
;; --------------------------------------------------
(after! dap-mode
  (require 'dap-codelldb)

  ;; This will fetch the latest codelldb extension, unpack it under
  ;; ~/.emacs.d/.local/cache/.extension/vscode/codelldb/
  (dap-codelldb-setup)

  (dap-register-debug-template
   "Rust::codelldb"
   (list :type        "lldb"
         :request     "launch"
         :name        "Rust :: codelldb Debug"
         :program     "${workspaceFolder}/target/debug/${workspaceFolderBasename}"
         :cwd         "${workspaceFolder}"
         :stopOnEntry t)))


;; --------------------------------------------------
;; TRAMP
;; --------------------------------------------------
(setq remote-file-name-inhibit-locks t
      tramp-use-scp-direct-remote-copying t
      remote-file-name-inhibit-auto-save-visited t
      tramp-copy-size-limit (* 1024 1024)  ; 1MB threshold for inline copying
      tramp-verbose 2)

;; Enable direct async processes for faster operations (e.g., Magit)
(connection-local-set-profile-variables
 'remote-direct-async-process
 '((tramp-direct-async-process . t)))
(connection-local-set-profiles
 '(:application tramp :machine "your-remote-host")
 'remote-direct-async-process)
(setq magit-tramp-pipe-stty-settings 'pty)

;; --------------------------------------------------
;; 42 SUTD
;; --------------------------------------------------
(setq user42 "yiyuli")
(setq mail42 "yy@eyuan.me")
(load! "lisp/42-header")
(map! "C-c h" #'stdheader)


;; --------------------------------------------------
;; Turn off auto complection
;; --------------------------------------------------
(map! "C-x c" (lambda () (interactive) (company-mode)))

;; --------------------------------------------------
;; Add C-SPC (and C-@ for ttys) as additional Evil window prefix
;; --------------------------------------------------
(after! evil
  (dolist (km '(evil-normal-state-map evil-motion-state-map))
    (define-key (symbol-value km) (kbd "C-SPC") evil-window-map)
    (define-key (symbol-value km) (kbd "C-@")   evil-window-map)))

;; --------------------------------------------------
;; Auto-Swith theme
;; --------------------------------------------------
;; (use-package! auto-dark
;;   :defer t
;;   :init
;;   (setq! auto-dark-themes '((doom-one) (doom-tomorrow-day)))
;;   ;; Disable doom's theme loading mechanism (just to make sure)
;;   (setq! doom-theme nil)
;;   ;; Declare that all themes are safe to load.
;;   ;; Be aware that setting this variable may have security implications if you
;;   ;; get tricked into loading untrusted themes (via auto-dark-mode or manually).
;;   ;; See the documentation of custom-safe-themes for details.
;;   (setq! custom-safe-themes t)
;;   ;; Enable auto-dark-mode at the right point in time.
;;   ;; This is inspired by doom-ui.el. Using server-after-make-frame-hook avoids
;;   ;; issues with an early start of the emacs daemon using systemd, which causes
;;   ;; problems with the DBus connection that auto-dark mode relies upon.
;;   (defun my-auto-dark-init-h ()
;;     (auto-dark-mode)
;;     (remove-hook 'server-after-make-frame-hook #'my-auto-dark-init-h)
;;     (remove-hook 'after-init-hook #'my-auto-dark-init-h))
;;   (let ((hook (if (daemonp)
;;                   'server-after-make-frame-hook
;;                 'after-init-hook)))
;;     ;; Depth -95 puts this before doom-init-theme-h, which sounds like a good
;;     ;; idea, if only for performance reasons.
;;     (add-hook hook #'my-auto-dark-init-h -95)))

;; --------------------------------------------------
;; lsp shit
;; --------------------------------------------------
(after! c-ts-mode
  (add-hook 'c++-ts-mode-hook #'clang-format-on-save-mode))

(use-package! claude-code-ide
  :defer t
  ;; :commands (claude-code-ide-start claude-code-ide-some-command) ; if known
  ;; :init (setq claude-code-ide-option t) ; set variables before loading
  ;; :config
  ;; (add-hook 'some-mode-hook #'claude-code-ide-mode)
  )

(ultra-scroll-mode 1)
(setq claude-code-ide-terminal-backend 'vterm)
(setq claude-code-ide-vterm-anti-flicker t)
(setq claude-code-ide-terminal-initialization-delay 0)
