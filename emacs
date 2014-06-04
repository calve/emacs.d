;;Various found stuff
(add-to-list 'load-path "~/.emacs.d")


(normal-top-level-add-to-load-path '("."))
(normal-top-level-add-subdirs-to-load-path)

;;This key will kill the active buffer without any prompting whatsoever.
(defun kill-this-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

;; Key binding
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "M-g") 'magit-status)
(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key (kbd "<backtab>") 'other-window)
(global-set-key (kbd "C-é") 'control-mode)
(global-set-key (kbd "C-ç") 'other-window)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-x") 'smex)

;; Indent with spaces only
(setq-default indent-tabs-mode nil)
;;(setq-default show-trailing-whitespace t)

;;Chargement du theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'Arthuremacstheme t)
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

;;All the packages i want
;; set local recipes, el-get-sources should only accept PLIST element
(setq
 el-get-sources
 '((:name smex; a better (ido like) M-x
        :after (lambda ()
                 (setq smex-save-file "~/.emacs.d/.smex-items")
                 (global-set-key (kbd "M-x") 'smex)
                 (global-set-key (kbd "M-X") 'smex-major-mode-commands)))

 (:name magit; git meet emacs, and a binding
        :after (lambda ()
                 (global-set-key (kbd "M-g") 'magit-status))))
)

;; now set our own packages
(setq
 my:el-get-packages
 '(el-get; el-get is self-hosting
   company-mode
   autopair
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
   php-eldoc
   php-mode
   pkgbuild-mode
   popup
   rainbow-mode
   tuareg-mode
   xterm-color
   yasnippet
   web-mode
   ))

(setq my:el-get-packages
      (append
       my:el-get-packages
       (loop for src in el-get-sources collect (el-get-source-name src))))

;; install new packages and init already installed packages
(el-get 'sync my:el-get-packages)

;; ;; Now we can load elpa stuff
;; Load company mode for every buffer
(add-hook 'after-init-hook 'global-company-mode)

;;Show function prototype
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)

;;autopair is about automagically insert closing parenthesis
(require 'autopair)
(autopair-global-mode 1)

;;pkgbuild is how to construct an arch linux package
(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
(setq auto-mode-alist (append '(("/PKGBUILD$" . pkgbuild-mode))
                               auto-mode-alist))
;; print black on white
(setq ps-default-fg t)
(setq ps-default-bg t)

;; control-mode, like caps lock but for modifiers

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

;; python eldoc and completion
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc)
(add-hook 'python-mode-hook (add-to-list 'company-backends 'company-anaconda))


;; whitespaces configuration, mostly show tabulations
(setq whitespace-style (quote (face tabs tab-mark trailing empty)))
(global-whitespace-mode)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-quick-help-delay 0.2)
 '(ac-use-fuzzy t)
 '(custom-safe-themes (quote ("52712f2b6807d4ba4985cc8df0c1a186846e87c7fcf6d43a5c84d6584c5f4ad3" "2fbaf3d9682f8d0d08262e062e97dad1ef062622f8ebfdb13098fcd7a7d76436" default)))
 '(magit-diff-options nil)
 '(ps-default-fg "black" t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(global-unset-key (kbd "<backtab>"))
(global-set-key (kbd "<backtab>") 'other-window)

