(deftheme Arthuremacstheme
  "Created 2013-10-30.")

(custom-theme-set-faces
 'Arthuremacstheme
 '(default ((t (:foreground "#808080"))))
 '(cursor ((t (:background "#FFFFFF"))))
 '(fixed-pitch ((t (:family "Monospace"))))
 '(variable-pitch ((t (:family "Sans Serif"))))
 '(escape-glyph ((t (:foreground "brightgreen"))))
 '(minibuffer-prompt ((t (:foreground "brightblue"))))
 '(highlight ((t (:background "darkseagreen2" :foreground "black"))))
 '(region ((t (:background "color-16"))))
 '(shadow ((((class color grayscale) (min-colors 88) (background light)) (:foreground "grey50")) (((class color grayscale) (min-colors 88) (background dark)) (:foreground "grey70")) (((class color) (min-colors 8) (background light)) (:foreground "green")) (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))))
 '(secondary-selection ((((class color) (min-colors 88) (background light)) (:background "yellow1")) (((class color) (min-colors 88) (background dark)) (:background "SkyBlue4")) (((class color) (min-colors 16) (background light)) (:background "yellow")) (((class color) (min-colors 16) (background dark)) (:background "SkyBlue4")) (((class color) (min-colors 8)) (:foreground "black" :background "cyan")) (t (:inverse-video t))))
 '(trailing-whitespace ((t (:underline t :foreground "#d70000" :background nil))))
 '(whitespace-trailing ((t (:underline t :foreground "#00afaf" :background nil))))
 '(whitespace-tab ((t (:underline t :foreground "#00afaf" :background nil))))
 '(whitespace-space ((t (:underline t :foreground "#00afaf" :background nil))))
 '(whitespace-empty ((t (:underline t :foreground "#585858" :background nil))))

 '(font-lock-builtin-face ((t (:foreground "#af005f"))))
 '(font-lock-comment-delimiter-face ((default (:inherit (font-lock-comment-face)))))
 '(font-lock-comment-face ((t (:foreground "#585858"))))
 '(font-lock-constant-face ((t (:foreground "#00afaf"))))
 '(font-lock-doc-face ((t (:inherit (font-lock-string-face)))))
 '(font-lock-function-name-face ((t (:foreground "#d75f00"))))
 '(font-lock-keyword-face ((t (:weight bold :foreground "#5f8700"))))
 '(font-lock-negation-char-face ((t nil)))
 '(font-lock-preprocessor-face ((t (:weight bold :foreground "#af8700"))))
 '(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
 '(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
 '(font-lock-string-face ((t (:foreground "#00afaf"))))
 '(font-lock-type-face ((t (:weight bold :foreground "#af8700"))))
 '(font-lock-variable-name-face ((t (:foreground "#0087ff"))))
 '(font-lock-warning-face ((t (:inherit (error)))))

 '(button ((t (:inherit (link)))))
 '(link ((((class color) (min-colors 88) (background light)) (:underline (:color foreground-color :style line) :foreground "RoyalBlue3")) (((class color) (background light)) (:underline (:color foreground-color :style line) :foreground "blue")) (((class color) (min-colors 88) (background dark)) (:underline (:color foreground-color :style line) :foreground "cyan1")) (((class color) (background dark)) (:underline (:color foreground-color :style line) :foreground "cyan")) (t (:inherit (underline)))))
 '(link-visited ((default (:inherit (link))) (((class color) (background light)) (:foreground "magenta4")) (((class color) (background dark)) (:foreground "violet"))))
 '(fringe ((t (:background "grey95" :foreground "black"))))
 '(tooltip ((((class color)) (:inherit (variable-pitch) :foreground "black" :background "lightyellow")) (t (:inherit (variable-pitch)))))

 ;;Diff faces
 '(diff-added ((t (:inherit diff-changed :foreground "#859900" :background "#262626"))))
 '(diff-header ((t (:background "white" :foreground "brightwhite"))))
 '(diff-removed ((t (:inherit diff-changed :foreground "#d70000" :background "#262626"))))

 '(ediff-even-diff-A ((t (:inherit diff-added))))
 '(diff-header ((t (:background "white" :foreground "brightwhite"))))
 '(ediff-even-diff-B ((t (:inherit diff-removed))))

 '(header-line ((t (:background "grey10"  :foreground "#0087ff"))))
 '(git-commit-summary-face ((t (:foreground "brightblue"))))
 '(magit-diff-none ((t (:inherit diff-context :foreground "black"))))
 '(magit-item-highlight ((t (:inherit secondary-selection :background "brightblack" :foreground "black"))))
 '(region ((t (:inverse-video t))))
 '(rng-error ((t (:inherit font-lock-warning-face :foreground "red"))))
 '(secondary-selection ((t (:background "yellow" :foreground "black"))))
 '(woman-bold ((t (:inherit bold :foreground "brightblue"))))
 '(woman-italic ((t (:inherit italic :foreground "brightgreen"))))

;; mode line faces
 '(mode-line ((t (:background "black" :foreground "#0087ff"))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-emphasis ((t (:weight bold))))
; '(mode-line-highlight ((((class color) (min-colors 88)) (:box (:line-width 2 :color "grey40" :style released-button))) (t (:inherit (highlight)))))
 '(mode-line-inactive ((t (:background "grey10" :foreground "#808080"))))

 '(isearch ((((class color) (min-colors 88) (background light)) (:foreground "lightskyblue1" :background "magenta3")) (((class color) (min-colors 88) (background dark)) (:foreground "brown4" :background "palevioletred2")) (((class color) (min-colors 16)) (:foreground "cyan1" :background "magenta4")) (((class color) (min-colors 8)) (:foreground "cyan1" :background "magenta4")) (t (:inverse-video t))))
 '(isearch-fail ((((class color) (min-colors 88) (background light)) (:background "RosyBrown1")) (((class color) (min-colors 88) (background dark)) (:background "red4")) (((class color) (min-colors 16)) (:background "red")) (((class color) (min-colors 8)) (:background "red")) (((class color grayscale)) (:foreground "grey")) (t (:inverse-video t))))
 '(lazy-highlight ((t (:background "magenta" :foreground "black"))))
 '(match ((t (:background "yellow1" :foreground "color-18"))))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch)))))
 '(tuareg-font-lock-operator-face ((t (:foreground "brightyellow"))))

;; On the fly rrors
 '(flymake-errline ((t (:background nil :foreground "#d70000"))))
 '(flymake-warnline ((t (:background nil :foreground "#d75f00"))))
 '(flycheck-error ((t (:underline t :background nil :foreground "#d70000"))))
 '(flycheck-warning ((t (:underline t  :background nil :foreground "#d75f00"))))

;;Tuareg faces
'(tuareg-font-lock-operator-face ((t (:foreground "#7070FF"))))

;;web mode
'(web-mode-html-tag-face ((t (:foreground "#BBBBBB"))))
'(web-mode-html-attr-name-face ((t (:foreground "#AAAAAA"))))
'(web-mode-html-tag-bracket-face ((t (:foreground "#AAAAAA"))))
;; '(default ((t (:family "default" :foundry "default" :width normal :height 1 :weight normal :slant normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :foreground "#FFFFFF" :stipple nil :inherit nil)))))
)
(provide-theme 'Arthuremacstheme)
