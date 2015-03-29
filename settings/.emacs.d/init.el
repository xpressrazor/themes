(package-initialize)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

;; Load path
(add-to-list 'load-path "~/.emacs.d/elpa")
(add-to-list 'load-path "~/.emacs.d/lisp")

;; Recent files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; Enable dirtree
(require 'dirtree)

;(defun ep-dirtree ()
;  (interactive)
;  (dirtree-in-buffer eproject-root t))
(global-set-key "\C-o" 'dirtree-show)

(global-set-key (kbd "<C-up>") 'shrink-window)
(global-set-key (kbd "<C-down>") 'enlarge-window)
(global-set-key (kbd "<C-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-right>") 'enlarge-window-horizontally)


;; Enable projectile
(projectile-global-mode)
(setq projectile-enable-caching t)
(setq projectile-require-project-root nil)
(setq projectile-completion-system 'grizzl)

;; Ruby
(add-hook 'ruby-mode-hook
 (lambda ()
 (defadvice ruby-mode-set-encoding
 (around ruby-mode-set-encoding-disable activate) nil)))




(add-hook 'ruby-mode-hook 'robe-mode)
(require 'haml-mode)

(setq robe-mode-disable-auto-pairing nil)



;; git
(setq git-state-modeline-decoration 'git-state-decoration-large-dot)
(global-set-key (kbd "C-x M-g") 'git-status)

;; General Settings
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-splash-screen t)
(switch-to-buffer "**")

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(when (fboundp 'winner-mode)
  (winner-mode 1))

(global-set-key [f9] 'toggle-menu-bar-mode-from-frame)
(setq inhibit-splash-screen t)
;(set-default-font "Inconsolata-g 11")
(set-default-font "Source Code Pro 10")
;(set-default-font "Ubuntu 11")
;(set-default-font "Consolas 11")

;; Enable easy switch
(require 'ido)
(ido-mode 'buffers) ;; only use this line to turn off ido for file names!
(setq ido-ignore-buffers '("^ " "*Completions*" "*Shell Command Output*"
			   "*Messages*" "Async Shell Command"))
(setq ido-separator "\n")

;; Packages

;; QuickRun
;;(require 'quickrun)

(global-set-key (kbd "<f7>") 'quickrun)
(global-set-key (kbd "<f8>") 'quickrun-compile-only)



(setq path-to-ctags "/usr/bin/ctags") ;; <- your ctags path here


;; web-mode
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode)) 
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode)) 
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode)) 
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode)) 
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode)) 
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

(defun my-web-mode-hook () 
"Hooks for Web mode." 
(setq web-mode-markup-indent-offset 2) 
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-style-padding 1)
(setq web-mode-script-padding 1)
(setq web-mode-block-padding 0)
(setq web-mode-comment-style 2)
(define-key web-mode-map (kbd "C-.") 'web-mode-tag-match)

(setq web-mode-extra-auto-pairs '(("erb" . (("beg" "end"))) ("php" . (("beg" "end") ("beg" "end"))) ))

(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-css-colorization t)
(setq web-mode-enable-block-face t)
(setq web-mode-enable-part-face t)
(setq web-mode-enable-comment-keywords t)
(setq web-mode-enable-heredoc-fontification t)
(setq web-mode-enable-current-element-highlight t)
(setq web-mode-enable-current-column-highlight t)


) 

(add-hook 'web-mode-hook 'my-web-mode-hook)

(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "ctags -f %s -e -R %s" path-to-ctags (directory-file-name dir-name)))
  )


;; org mode
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)


;; Auto-complete
(require 'auto-complete)
(global-auto-complete-mode t)
(add-to-list 'ac-modes 'sql-mode)

(ac-flyspell-workaround)
 
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d//ac-dict")
(ac-config-default)

(ac-ropemacs-initialize)
(global-auto-complete-mode t)
(define-key ac-complete-mode-map "\t" 'ac-expand)
(define-key ac-complete-mode-map "\r" 'ac-complete)
(define-key ac-complete-mode-map "\M-n" 'ac-next)
(define-key ac-complete-mode-map "\M-p" 'ac-previous)
(setq ac-auto-start 3)
(setq ac-dwim t)
(set-default 'ac-sources '(ac-source-abbrev ac-source-words-in-buffer))
(setq ac-modes 
    (append ac-modes
        '(eshell-mode
)))

;; Rsense + Autocomplete
(add-hook 'ruby-mode-hook
  (lambda ()
    (add-to-list 'ac-sources 'ac-source-rsense-method)
    (add-to-list 'ac-sources 'ac-source-rsense-constant)))

;;
;(add-hook 'ruby-mode-hook
;          (lambda ()
;             (autopair-mode -1)
;             (ruby-electric-mode t)))




;; dirty fix for having AC everywhere
(define-globalized-minor-mode real-global-auto-complete-mode
  auto-complete-mode (lambda ()
		       (if (not (minibufferp (current-buffer)))
			   (auto-complete-mode 1))
		       ))
(real-global-auto-complete-mode t)



;; Auto-indent
(add-hook 'lisp-mode-hook '(lambda ()
     (local-set-key (kbd "RET") 'newline-and-indent)))
(define-key global-map (kbd "RET") 'newline-and-indent)


;; turn on automatic bracket insertion by pairs. New in emacs 24
(electric-pair-mode 1)
