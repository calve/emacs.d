;;Various found stuff
(add-to-list 'load-path "~/.emacs.d")


;;(normal-top-level-add-to-load-path '("."))
;;(normal-top-level-add-subdirs-to-load-path)

;;This key will kill the active buffer without any prompting whatsoever.
(defun kill-this-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

;; Key binding
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c f") 'flyspell-buffer)
(global-set-key (kbd "C-c SPC") 'whitespace-cleanup)
(global-set-key (kbd "M-g") 'magit-status)
(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key (kbd "<backtab>") 'other-window)
(global-set-key (kbd "C-é") 'control-mode)
(global-set-key (kbd "C-ç") 'other-window)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-.") 'resize-window)

;; Indent with spaces only
(setq-default indent-tabs-mode nil)
;;(setq-default show-trailing-whitespace t)

;;Chargement du theme
(load-file "~/.emacs.d/themes/Arthur-modeline.el")

(menu-bar-mode -1)
(column-number-mode 1)
(line-number-mode 1)
(show-paren-mode 1)
(setq inhibit-startup-message t)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(setq split-height-threshold 0)
(setq split-width-threshold 50)
(setq window-min-width 30)

;; Better buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers)

;; Open a file by sudo over ssh
;; C-x C-f /sudo:root@host[#port]:/path/to/file
;(set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:"))))
(set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/sshx:%h:"))))
(setq-default tramp-default-method "sshx")
;; only use bash, zsh is useless with tramp
(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
;; autosave on localhost
(setq tramp-auto-save-directory "~/emacs/tramp-autosave")


(defun window-toggle-split-direction ()
  "Switch window split from horizontally to vertically, or vice versa.
i.e. change right window to bottom, or change bottom window to right."
  ;(interactive)
  (require 'windmove)
  (let ((done))
    (dolist (dirs '((right . down) (down . right)))
      (unless done
        (let* ((win (selected-window))
               (nextdir (car dirs))
               (neighbour-dir (cdr dirs))
               (next-win (windmove-find-other-window nextdir win))
               (neighbour1 (windmove-find-other-window neighbour-dir win))
               (neighbour2 (if next-win (with-selected-window next-win
                                          (windmove-find-other-window neighbour-dir next-win)))))
          ;;(message "win: %s\nnext-win: %s\nneighbour1: %s\nneighbour2:%s" win next-win neighbour1 neighbour2)
          (setq done (and (eq neighbour1 neighbour2)
                          (not (eq (minibuffer-window) next-win))))
          (if done
              (let* ((other-buf (window-buffer next-win)))
                (delete-window next-win)
                (if (eq nextdir 'right)
                    (split-window-vertically)
                  (split-window-horizontally))
                (set-window-buffer (windmove-find-other-window neighbour-dir) other-buf))))))))

;; Resize window interactively using tsrn
(defun resize-window (&optional arg)    ; Hirose Yuuji and Bob Wiener
  "*Resize window interactively."
  (interactive "p")
  (if (one-window-p) (error "Cannot resize sole window"))
  (or arg (setq arg 1))
  (let (c)
    (catch 'done
      (while t
        (message
         "h=heighten, s=shrink, w=widen, n=narrow (by %d);  1-9=unit, t=toogle split q=quit"
         arg)
        (setq c (read-char))
        (condition-case ()
            (cond
             ((= c ?h) (enlarge-window arg))
             ((= c ?s) (shrink-window arg))
             ((= c ?w) (enlarge-window-horizontally arg))
             ((= c ?n) (shrink-window-horizontally arg))
             ((= c ?.) (window-toggle-split-direction))
             ((= c ?\^G) (keyboard-quit))
             ((= c ?q) (throw 'done t))
             ((and (> c ?0) (<= c ?9)) (setq arg (- c ?0)))
             (t (beep)))
          (error (beep)))))
    (message "Done.")))

;; just-one-space in rectangles
(require 'rect)
(defun just-one-space-in-rect-line (start end)
  (save-restriction
    (save-match-data
      (narrow-to-region (+ (point) start)
                        (+ (point) end))
      (while (re-search-forward "\\s-+" nil t)
        (replace-match " ")))))
(defun just-one-space-in-rect (start end)
  "replace all whitespace in the rectangle with single spaces"
  (interactive "r")
  (apply-on-rectangle 'just-one-space-in-rect-line start end))

;; Use hunspell for grammar and syntax correction
(setq ispell-program-name "hunspell")

;;Interactively do things with buffers and files
(require 'ido)
(ido-mode t)

(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; bootstrap el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(require 'el-get-elpa)
;; Build the El-Get copy of the package.el packages if we have not
;; built it before.  Will have to look into updating later ...
(unless (file-directory-p el-get-recipe-path-elpa)
  (el-get-elpa-build-local-recipes))

(el-get 'sync)

;; now set our own packages
(setq
 my:el-get-packages
 '(el-get; el-get is self-hosting
   company-mode
   anaconda-mode
   c-eldoc
   color-theme                ; nice looking emacs
   color-theme-tango
   flycheck
   git-commit-mode
   git-rebase-mode
   god-mode
   js2-mode
   markdown-mode
   magit
   php-eldoc
   php-mode
   pkgbuild-mode
   popup
   rainbow-mode
   smartparens
   smex
   color-theme-solarized
   tuareg-mode
   xterm-color
   yasnippet
   web-mode
   ))

(setq my:el-get-packages
      (append
       my:el-get-packages
       (loop for src in el-get-sources collect (el-get-source-name src))))

;; Use solarized colors
(setq solarized-termcolors 256)

(load-theme 'solarized-dark t)
(setq solarized-diff-mode "high")

;; install new packages and init already installed packages
(el-get 'sync my:el-get-packages)

;; ;; Now we can load elpa stuff
;; Load company mode for every buffer
(add-hook 'after-init-hook 'global-company-mode)

;;Show function prototype
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)

;;pkgbuild is how to construct an arch linux package
(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
(setq auto-mode-alist (append '(("/PKGBUILD$" . pkgbuild-mode))
                               auto-mode-alist))
;; print black on white
(setq ps-default-fg t)
(setq ps-default-bg t)

;; disable spell checking on git-commit
(eval-after-load "git-commit-mode"
  '(cond
    ((boundp 'git-commit-mode-hook) ; old
     (remove-hook 'git-commit-mode-hook 'flyspell-mode))
    ((boundp 'git-commit-setup-hook) ; new
     (remove-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell))))

;; disable scss compilation on save
(setq scss-compile-at-save nil)

;; web mode is cool for editing html templates
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.twig\\'" . web-mode))
(setq-default web-mode-markup-indent-offset 2)

(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))

;; settings gud/gdb
(setq gdb-show-main t)

;; company is a completion mode using backends and frontends
;; we set here the behaviour of tab key. Hit it will :
;; indent the current line, then iterate over completion candidates
;; (defun complete-or-indent ()
;;   (interactive)
;;   (indent-according-to-mode)  
;;   (if (looking-at "\\_>")
;;       (company-complete-common))
;;   )

;; (global-set-key "\t" 'complete-or-indent)
(global-company-mode)
(setq company-selection-wrap-around t)

(add-hook 'after-init-hook #'global-flycheck-mode)

;; python eldoc and completion
(add-hook 'python-mode-hook 'eldoc-mode)
(add-hook 'python-mode-hook 'anaconda-mode)
(setq-default flycheck-flake8-maximum-line-length 120)


;; whitespaces configuration, mostly show tabulations
(setq whitespace-style (quote (face tabs tab-mark trailing empty)))
(global-whitespace-mode)


;; saltstack config file are usually yaml
(add-to-list 'auto-mode-alist '("\\.sls\\'" . yaml-mode))

;; tms prevention with god-mode
(require 'god-mode)
(global-set-key (kbd "œ") 'god-mode-all)
(global-set-key (kbd "M-'") 'god-mode-all)
(define-key god-local-mode-map (kbd "z") 'repeat)
(define-key god-local-mode-map (kbd "i") 'god-local-mode)
(defun god-mode-modeline ()
  (cond (god-local-mode (progn (set-face-background 'mode-line "#e4e4e4") 
                               (set-face-background 'mode-line-inactive "#ffffd7")))
        (t (progn (set-face-background 'mode-line "black")
                  (set-face-background 'mode-line-inactive "grey10")))))
(add-hook 'god-mode-enabled-hook 'god-mode-modeline)
(add-hook 'god-mode-disabled-hook 'god-mode-modeline)


;; stop insert closing parenthesis
(smartparens-global-mode)
(sp-local-pair 'web-mode "<%" "%>")
;;; markdown-mode
(sp-with-modes '(markdown-mode gfm-mode rst-mode)
  (sp-local-pair "*" "*")
  (sp-local-pair "'" nil :actions nil)
  (sp-local-tag "2" "**" "**")
  (sp-local-tag "s" "```scheme" "```")
  (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))

;;a try to swap home keys
(define-key key-translation-map "\C-b" "\C-s")
(define-key key-translation-map "\C-s" "\C-b")
(define-key key-translation-map "\C-f" "\C-r")
(define-key key-translation-map "\C-r" "\C-f")
(define-key key-translation-map "\C-t" "\C-p")
(define-key key-translation-map "\C-p" "\C-t")
(define-key god-local-mode-map (kbd "t") 'previous-line)
(define-key god-local-mode-map (kbd "s") 'backward-char)
(define-key god-local-mode-map (kbd "r") 'forward-char)
(define-key god-local-mode-map (kbd "p") 'transpose-char)
(define-key god-local-mode-map (kbd "b") 'isearch-forward)
(define-key god-local-mode-map (kbd "f") 'isearch-backward)
(global-set-key (kbd "C-x C-b") 'save-buffer) ;; That is translated C-x C-s
(global-set-key (kbd "C-x C-r") 'ido-find-file) ;; That is translated C-x C-f

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-quick-help-delay 0.2)
 '(ac-use-fuzzy t)
 '(custom-safe-themes (quote ("1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "52712f2b6807d4ba4985cc8df0c1a186846e87c7fcf6d43a5c84d6584c5f4ad3" "2fbaf3d9682f8d0d08262e062e97dad1ef062622f8ebfdb13098fcd7a7d76436" default)))
 '(magit-commit-popup-defaults nil)
 '(magit-diff-options nil)
 '(magit-log-popup-defaults (quote ("--graph" "--decorate" "--all")))
 '(ps-default-fg "black" t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:foreground "#808080" :background nil :foundry "default" :family "default"))))
 '(flycheck-error ((t (:foreground "#d70000" :underline t))))
 '(flycheck-warning ((t (:foreground "#d75f00" :underline t))))
 '(git-commit-summary-face ((t (:foreground "#0087ff"))))
 '(web-mode-html-attr-name-face ((t (:foreground "#585858"))))
 '(web-mode-html-tag-bracket-face ((t (:foreground "#585858"))))
 '(web-mode-html-tag-face ((t (:foreground "#585858"))))
 '(whitespace-tab ((t (:foreground "#1010FF" t))))
 '(whitespace-trailing ((t (:underline t))))
 )

(global-unset-key (kbd "<backtab>"))
(global-set-key (kbd "<backtab>") 'other-window)

