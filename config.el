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

;; --------------------------------------------------
;; Force symbols/emoji to render as text, not colorful emoji
;; --------------------------------------------------
;; Use Symbols Nerd Font or a monochrome font instead of Apple Color Emoji

(setq doom-font (font-spec :family "Berkeley Mono" :size 12)
      doom-variable-pitch-font (font-spec :family "Gill Sans" :size 13)
      doom-big-font (font-spec :family "Berkeley Mono" :size 17))

(after! doom-ui
  (set-fontset-font t 'emoji "Symbols Nerd Font Mono" nil 'prepend)
  (set-fontset-font t 'symbol "Symbols Nerd Font Mono" nil 'prepend)
  (set-fontset-font t 'emoji "Apple Symbols" nil 'append)
  (set-fontset-font t 'symbol "Apple Symbols" nil 'append))


;; Frameless window with native macOS rounded corners (emacs-plus patch)
(add-to-list 'default-frame-alist '(undecorated-round . t))

;; Translucent background (emacs-plus patch)
;; Value from 0 (fully transparent) to 100 (fully opaque)
(add-to-list 'default-frame-alist '(alpha-background . 75))

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

;; --------------------------------------------------
;; Nyan Cat modeline
;; --------------------------------------------------
(use-package! nyan-mode
  :after doom-modeline
  :config
  (setq nyan-animate-nyancat t
        nyan-wavy-trail t
        nyan-bar-length 20)
  (nyan-mode 1))

;; --------------------------------------------------
;; Modeline tweaks
;; --------------------------------------------------
;; (after! doom-modeline
;;   (setq doom-modeline-battery                  t
;;         lsp-modeline-code-actions-enable       nil
;;         doom-modeline-vcs                      t
;;         doom-modeline-vcs-max-length           20
;;         doom-modeline-modal-icon               nil
;;         doom-modeline-lsp                      nil
;;         doom-modeline-lsp-perform-update-on-save ni  
;;         doom-modeline-workspace-name           
;;         doom-modeline-bar-width                3))

;; (with-eval-after-load 'lsp-mode
;;   (setq lsp-modeline-diagnostics-enable nil
;;         lsp-modeline-workspace-status-enable nil))


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
  (setq org-agenda-files '("~/Documents/Notes/org/agenda.org"))
  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 1.5)))

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
(use-package! auto-dark
  :defer t
  :init
  (setq! auto-dark-detection-method 'osascript)
  (setq! auto-dark-themes '((doom-tomorrow-night-hc) (doom-tomorrow-day)))
  ;; Disable doom's theme loading mechanism (just to make sure)
  (setq! doom-theme nil)
  ;; Declare that all themes are safe to load.
  ;; Be aware that setting this variable may have security implications if you
  ;; get tricked into loading untrusted themes (via auto-dark-mode or manually).
  ;; See the documentation of custom-safe-themes for details.
  (setq! custom-safe-themes t)
  ;; Enable auto-dark-mode at the right point in time.
  ;; This is inspired by doom-ui.el. Using server-after-make-frame-hook avoids
  ;; issues with an early start of the emacs daemon using systemd, which causes
  ;; problems with the DBus connection that auto-dark mode relies upon.
  (defun my-auto-dark-init-h ()
    (auto-dark-mode)
    (remove-hook 'server-after-make-frame-hook #'my-auto-dark-init-h)
    (remove-hook 'after-init-hook #'my-auto-dark-init-h))
  (let ((hook (if (daemonp)
                  'server-after-make-frame-hook
                'after-init-hook)))
    ;; Depth -95 puts this before doom-init-theme-h, which sounds like a good
    ;; idea, if only for performance reasons.
    (add-hook hook #'my-auto-dark-init-h -95)))

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

;; --------------------------------------------------
;; vterm ESC binding - C-ESC sends escape to terminal
;; --------------------------------------------------
(after! vterm
  (define-key vterm-mode-map (kbd "C-<escape>") #'vterm-send-escape)
  (define-key vterm-mode-map (kbd "C-M-[") #'vterm-send-escape))

;; --------------------------------------------------
;; eee.el - launch TUI tools (fzf, yazi, lazygit, etc.) in external terminal
;; --------------------------------------------------
(use-package! eee
  :commands (ee-find ee-rg ee-lazygit ee-yazi ee-yazi-project ee-recentf ee-line)
  :config
  (setq ee-terminal-command "/Applications/Alacritty.app/Contents/MacOS/alacritty"))

(after! eee
  (ee-define "ee-find"
    (ee-get-project-dir-or-current-dir)
    (expand-file-name "scripts/eee-find-local.sh" doom-user-dir)
    (list (ee-region-text))
    ee-jump-from))

;; Replace SPC SPC with eee fzf for project file search
(after! doom-keybinds
  (map! :leader
        :desc "Find file in project (fzf)" "SPC" #'ee-find))

;; --------------------------------------------------
;; EAF (removed)
;; --------------------------------------------------

;; --------------------------------------------------
;; Winpulse - flash active window on focus
;; --------------------------------------------------
(use-package! winpulse
  :config
  (winpulse-mode +1))

;; --------------------------------------------------
;; Linear.app
;; --------------------------------------------------
(use-package! linear-app
  :defer t
  :commands (linear-app))

(map! :leader
      :desc "Linear issues" "l i" #'linear-app)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((racket . t)))

(setq org-babel-default-header-args:racket
      '((:lang . "sicp")))

;; --------------------------------------------------
;; Eat - Emulate A Terminal
;; --------------------------------------------------
(use-package! eat
  :commands (eat eat-project)
  :config
  ;; Enable shell integration for better directory tracking
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)
  ;; Close buffer when process exits
  (setq eat-kill-buffer-on-exit t)
  ;; Use xterm-256color as TERM for better compatibility
  (setq eat-term-name "xterm-256color")
  ;; Enable shell integration (sends EAT_SHELL_INTEGRATION_DIR to shell)
  (eat-compile-terminfo))

(setenv "CLAUDECODE" nil)

