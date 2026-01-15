;;; doom-ghostty-theme.el --- A soft, muted dark theme -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added: 2025-12-30
;; Author: Yiyuan Li <yy@eyuan.me>
;; Maintainer: Yiyuan Li
;; Source: Ported from terminal color scheme
;;
;;; Commentary:
;; A dark theme with soft, muted colors featuring pastel accents.
;; Colors: soft pink, mint green, golden yellow, periwinkle blue, lavender, turquoise.
;;
;;; Code:

(require 'doom-themes)

;;; Variables
(defgroup doom-ghostty-theme nil
  "Options for the `doom-ghostty' theme."
  :group 'doom-themes)

(defcustom doom-ghostty-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-ghostty-theme
  :type 'boolean)

(defcustom doom-ghostty-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-ghostty-theme
  :type 'boolean)

(defcustom doom-ghostty-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line."
  :group 'doom-ghostty-theme
  :type '(choice integer boolean))

;;; Theme definition
(def-doom-theme doom-ghostty
  "A soft, muted dark theme with pastel accents."

  ;; Color definitions - BRIGHTER VERSION
  ((bg         '("#1b1c21" "#1a1a1a" "black"))
   (fg         '("#f8faff" "#ffffff" "brightwhite"))  ; Much brighter
   (bg-alt     '("#232429" "#222222" "black"))
   (fg-alt     '("#c6c9d1" "#c0c0c0" "white"))  ; Brighter

   ;; Brightness spectrum (base0 = darkest, base8 = brightest)
   (base0      '("#0c0e12" "#000000" "black"))
   (base1      '("#18191e" "#1a1a1a" "brightblack"))
   (base2      '("#232429" "#252525" "brightblack"))
   (base3      '("#2e2f35" "#303030" "brightblack"))
   (base4      '("#43454b" "#454545" "brightblack"))  ; Brighter line numbers
   (base5      '("#62646b" "#606060" "brightblack"))  ; Brighter comments
   (base6      '("#8b8d94" "#888888" "brightblack"))  ; Brighter
   (base7      '("#b2b5bd" "#b0b0b0" "brightblack"))  ; Brighter
   (base8      '("#f8faff" "#ffffff" "white"))

   ;; Semantic colors - BRIGHTER
   (grey       base5)
   (red        '("#ffd5da" "#ffaaaa" "red"))  ; Using bright variant
   (orange     '("#ffcab8" "#ffbb99" "brightred"))  ; Brighter
   (green      '("#bdf0c4" "#aaffaa" "green"))  ; Using bright variant
   (teal       '("#7eeee8" "#77eeee" "brightgreen"))  ; Brighter
   (yellow     '("#ffdc99" "#ffdd88" "yellow"))  ; Using bright variant
   (blue       '("#d5e1ff" "#ccdeff" "brightblue"))  ; Using bright variant
   (dark-blue  '("#3a455f" "#335588" "blue"))
   (magenta    '("#ebd8ff" "#eeccff" "brightmagenta"))  ; Using bright variant
   (violet     '("#f5ebff" "#ffeeFF" "magenta"))  ; Even brighter
   (cyan       '("#7eeee8" "#77eeee" "brightcyan"))  ; Brighter
   (dark-cyan  '("#00652d" "#006633" "cyan"))

   ;; Extra bright variants for emphasis
   (bright-red     '("#ffe5e8" "#ffdddd" "red"))
   (bright-green   '("#d0f5d5" "#ccffcc" "green"))
   (bright-yellow  '("#ffebb5" "#ffeeaa" "yellow"))
   (bright-blue    '("#e5edff" "#eef5ff" "brightblue"))
   (bright-magenta '("#f5ebff" "#ffeeff" "magenta"))
   (bright-cyan    '("#c5f8f5" "#ccffff" "cyan"))

   ;; Universal syntax classes (required)
   (highlight      blue)
   (vertical-bar   (doom-darken base1 0.1))
   (selection      dark-blue)
   (builtin        magenta)
   (comments       (if doom-ghostty-brighter-comments base6 base5))
   (doc-comments   (doom-lighten (if doom-ghostty-brighter-comments base6 base5) 0.25))
   (constants      violet)
   (functions      cyan)
   (keywords       blue)
   (methods        teal)
   (operators      magenta)
   (type           yellow)
   (strings        green)
   (variables      bright-blue)
   (numbers        orange)
   (region         `(,(car dark-blue) ,@(cdr base1)))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; Mode-line
   (-modeline-pad
    (when doom-ghostty-padded-modeline
      (if (integerp doom-ghostty-padded-modeline)
          doom-ghostty-padded-modeline
        4)))
   (modeline-bg    (if doom-ghostty-brighter-modeline
                       (doom-darken blue 0.45)
                     (doom-darken bg-alt 0.1)))
   (modeline-bg-alt (doom-darken bg-alt 0.15))
   (modeline-fg    fg)
   (modeline-fg-alt base5))

  ;; Face customizations
  (((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg :weight 'bold)

   ;; Cursor
   (cursor :background bright-blue)

   ;; Font lock (syntax highlighting)
   (font-lock-comment-face :foreground comments :slant 'italic)
   (font-lock-doc-face :foreground doc-comments)
   (font-lock-keyword-face :foreground keywords :weight 'bold)
   (font-lock-constant-face :foreground constants)
   (font-lock-function-name-face :foreground functions)
   (font-lock-string-face :foreground strings)
   (font-lock-type-face :foreground type)
   (font-lock-variable-name-face :foreground variables)
   (font-lock-builtin-face :foreground builtin)
   (font-lock-number-face :foreground numbers)
   (font-lock-warning-face :foreground warning)

   ;; Mode line
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-alt :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
   (mode-line-emphasis :foreground highlight)

   ;; Doom modeline
   (doom-modeline-bar :background highlight)
   (doom-modeline-buffer-file :weight 'bold)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-project-root :foreground green :weight 'bold)

   ;; Selection/region
   (region :background region :foreground nil)
   (lazy-highlight :background (doom-darken highlight 0.4) :foreground fg :weight 'bold)

   ;; Search
   (isearch :background highlight :foreground base0 :weight 'bold)
   (isearch-fail :background red :foreground base0)

   ;; Links
   (link :foreground cyan :underline t)
   (link-visited :foreground violet :underline t)

   ;; Minibuffer
   (minibuffer-prompt :foreground highlight)

   ;; Highlight line
   (hl-line :background base2)

   ;; Vertical border
   (vertical-border :foreground base4)

   ;; Fringe
   (fringe :background bg :foreground base4)

   ;; Window divider
   (window-divider :foreground base4)
   (window-divider-first-pixel :foreground base4)
   (window-divider-last-pixel :foreground base4)

   ;; Ivy/Vertico/Consult
   (ivy-current-match :background region :weight 'bold)
   (ivy-minibuffer-match-face-1 :foreground cyan)
   (ivy-minibuffer-match-face-2 :foreground magenta :weight 'bold)
   (vertico-current :background region)

   ;; Company
   (company-tooltip :background base2 :foreground fg)
   (company-tooltip-selection :background region :foreground fg)
   (company-tooltip-common :foreground highlight :weight 'bold)
   (company-tooltip-annotation :foreground base6)
   (company-scrollbar-bg :background base2)
   (company-scrollbar-fg :background base5)

   ;; Treemacs
   (treemacs-root-face :foreground highlight :weight 'bold :height 1.2)
   (treemacs-file-face :foreground fg)
   (treemacs-directory-face :foreground blue)
   (treemacs-git-modified-face :foreground orange)
   (treemacs-git-added-face :foreground green)
   (treemacs-git-untracked-face :foreground cyan)

   ;; Magit
   (magit-section-heading :foreground blue :weight 'bold)
   (magit-branch-local :foreground cyan)
   (magit-branch-remote :foreground green)
   (magit-diff-added :foreground green :background (doom-blend green bg 0.1))
   (magit-diff-added-highlight :foreground bright-green :background (doom-blend green bg 0.2))
   (magit-diff-removed :foreground red :background (doom-blend red bg 0.1))
   (magit-diff-removed-highlight :foreground bright-red :background (doom-blend red bg 0.2))
   (magit-diff-context-highlight :background base2)

   ;; Org mode
   (org-level-1 :foreground blue :weight 'bold :height 1.3)
   (org-level-2 :foreground magenta :weight 'bold :height 1.2)
   (org-level-3 :foreground cyan :weight 'bold :height 1.1)
   (org-level-4 :foreground yellow :weight 'bold)
   (org-level-5 :foreground green)
   (org-level-6 :foreground orange)
   (org-level-7 :foreground red)
   (org-level-8 :foreground violet)
   (org-todo :foreground orange :weight 'bold)
   (org-done :foreground green :weight 'bold)
   (org-headline-done :foreground base5)
   (org-link :foreground cyan :underline t)
   (org-code :foreground yellow :background base2)
   (org-block :background (doom-darken base1 0.125))
   (org-block-begin-line :foreground base5 :background (doom-darken base1 0.125))
   (org-block-end-line :inherit 'org-block-begin-line)
   (org-table :foreground violet)
   (org-date :foreground cyan)

   ;; Markdown
   (markdown-header-face :foreground blue :weight 'bold)
   (markdown-code-face :background base2)
   (markdown-inline-code-face :foreground yellow :background base2)
   (markdown-link-face :foreground cyan)
   (markdown-url-face :foreground base6)

   ;; Dired
   (dired-directory :foreground blue :weight 'bold)
   (dired-symlink :foreground cyan)
   (dired-ignored :foreground base5)

   ;; Flycheck / Flymake
   (flycheck-error :underline `(:style wave :color ,red))
   (flycheck-warning :underline `(:style wave :color ,yellow))
   (flycheck-info :underline `(:style wave :color ,cyan))

   ;; LSP
   (lsp-face-highlight-read :background (doom-blend blue bg 0.3))
   (lsp-face-highlight-write :background (doom-blend orange bg 0.3))
   (lsp-face-highlight-textual :background (doom-blend green bg 0.2))

   ;; Which-key
   (which-key-key-face :foreground green)
   (which-key-command-description-face :foreground fg)
   (which-key-group-description-face :foreground magenta)
   (which-key-separator-face :foreground base5)

   ;; Diff-hl
   (diff-hl-change :foreground orange :background (doom-blend orange bg 0.2))
   (diff-hl-insert :foreground green :background (doom-blend green bg 0.2))
   (diff-hl-delete :foreground red :background (doom-blend red bg 0.2))

   ;; Rainbow delimiters
   (rainbow-delimiters-depth-1-face :foreground blue)
   (rainbow-delimiters-depth-2-face :foreground magenta)
   (rainbow-delimiters-depth-3-face :foreground cyan)
   (rainbow-delimiters-depth-4-face :foreground yellow)
   (rainbow-delimiters-depth-5-face :foreground green)
   (rainbow-delimiters-depth-6-face :foreground orange)
   (rainbow-delimiters-depth-7-face :foreground violet))

  ;; Variable overrides
  ())

;;; doom-ghostty-theme.el ends here
