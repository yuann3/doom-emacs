;;; doom-tomorrow-night-hc-theme.el --- High contrast Tomorrow Night -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Author: Forked from Henrik Lissner's doom-tomorrow-night
;; Source: https://github.com/ChrisKempson/Tomorrow-Theme
;;
;;; Commentary:
;; A high-contrast fork of doom-tomorrow-night with near-black background
;; and more saturated colors for better readability.
;;
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-tomorrow-night-hc-theme nil
  "Options for the `doom-tomorrow-night-hc' theme."
  :group 'doom-themes)

(defcustom doom-tomorrow-night-hc-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-tomorrow-night-hc-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme doom-tomorrow-night-hc
  "High contrast Tomorrow Night - darker background, brighter colors."
  :family 'doom-tomorrow
  :background-mode 'dark

  ;; name        gui       256       16
  ;; Darkened backgrounds for high contrast
  ((bg         '("#141414" nil       nil          ))  ; dark but not pure black
   (bg-alt     '("#0c0c0c" nil       nil          ))  ; slightly darker
   (base0      '("#000000" "black"   "black"      ))  ; pure black
   (base1      '("#0a0a0a" "#0a0a0a"              ))
   (base2      '("#121212" "#121212"              ))
   (base3      '("#1a1a1a" "#1a1a1a" "brightblack"))
   (base4      '("#333333" "#333333" "brightblack"))
   (base5      '("#555555" "#555555" "brightblack"))
   (base6      '("#7a7a7a" "#7a7a7a" "brightblack"))
   (base7      '("#a0a0a0" "#a0a0a0" "brightblack"))
   (base8      '("#ffffff" "#ffffff" "white"      ))
   ;; Brighter foreground for contrast against dark bg
   (fg         '("#e0e0e0" "#e0e0e0" "white"))       ; brighter (was #c5c8c6)
   (fg-alt     (doom-darken fg 0.3))

   ;; Brighter, more saturated colors
   (grey       '("#6a6b6a" "#6a6a6a" "brightblack"))
   (red        '("#e06666" "#e06666" "red"))         ; brighter (was #cc6666)
   (orange     '("#f0a050" "#f0a050" "brightred"))   ; brighter (was #de935f)
   (yellow     '("#f5d070" "#f5d070" "yellow"))      ; brighter (was #f0c674)
   (green      '("#c5cd78" "#c5cd78" "green"))       ; brighter (was #b5bd68)
   (blue       '("#90b5d5" "#90b5d5" "brightblue"))  ; brighter (was #81a2be)
   (dark-blue  '("#5188b0" "#5188b0" "blue"))        ; brighter (was #41728e)
   (teal       blue)
   (magenta    '("#dcc8e0" "#dcc8e0" "magenta"))     ; brighter (was #c9b4cf)
   (violet     '("#c8a8d0" "#c8a8d0" "brightmagenta")) ; brighter (was #b294bb)
   (cyan       '("#9ed0c8" "#9ed0c8" "cyan"))        ; brighter (was #8abeb7)
   (dark-cyan  (doom-darken cyan 0.4))

   ;; face categories
   (highlight      blue)
   (vertical-bar   base0)
   (selection      `(,(car (doom-lighten bg 0.15)) ,@(cdr base4)))
   (builtin        blue)
   (comments       grey)
   (doc-comments   (doom-lighten grey 0.2))
   (constants      orange)
   (functions      blue)
   (keywords       violet)
   (methods        blue)
   (operators      fg)
   (type           yellow)
   (strings        green)
   (variables      red)
   (numbers        orange)
   (region         selection)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    fg-alt)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (modeline-bg     `(,(doom-darken (car bg-alt) 0.2) ,@(cdr base3)))
   (modeline-bg-alt `(,(car bg) ,@(cdr base1)))
   (modeline-fg     base8)
   (modeline-fg-alt comments)
   (-modeline-pad
    (when doom-tomorrow-night-hc-padded-modeline
      (if (integerp doom-tomorrow-night-hc-padded-modeline)
          doom-tomorrow-night-hc-padded-modeline
        4))))

  ;; --- faces ------------------------------
  (((line-number &override) :foreground base5)  ; slightly brighter line numbers
   ((line-number-current-line &override) :foreground blue :bold bold)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-alt :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))

   ;;;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground violet)
   (rainbow-delimiters-depth-2-face :foreground blue)
   (rainbow-delimiters-depth-3-face :foreground orange)
   (rainbow-delimiters-depth-4-face :foreground green)
   (rainbow-delimiters-depth-5-face :foreground magenta)
   (rainbow-delimiters-depth-6-face :foreground yellow)
   (rainbow-delimiters-depth-7-face :foreground teal)
   ;;;; doom-modeline
   (doom-modeline-buffer-path       :foreground violet :bold bold)
   (doom-modeline-buffer-major-mode :inherit 'doom-modeline-buffer-path))

  ;; --- variables --------------------------
  ;; ()
  )

;;; doom-tomorrow-night-hc-theme.el ends here
