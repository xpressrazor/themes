(package-initialize)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

;; Load path
(add-to-list 'load-path "~/.emacs.d/elpa")
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

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
(require 'git)
(setq git-state-modeline-decoration 'git-state-decoration-large-dot)
(global-set-key (kbd "C-x M-g") 'git-status)

;; General Settings
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-splash-screen t)
(switch-to-buffer "**")
(show-paren-mode 1)

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

;; Haskell
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'inf-haskell-mode)



;; Bind l to help-go-back in help-mode
 (add-hook 'help-mode-hook
    (lambda () (define-key help-mode-map "l" 'help-go-back)))


;; Color-theme-select
(package-initialize)
(require 'color-theme)
(setq color-theme-is-global t)
(color-theme-initialize)
;(color-theme-classic)

;; Display time
(setq display-time-day-and-date t
      display-time-24hr-format t)
  (display-time)


;; Toggle - Maximized
(run-with-idle-timer 0.1 nil 'toggle-max-frame)
(global-set-key [f12] 'toggle-max-frame)

;; Reload ~/.emacs.d/init.el
(defun reload-init() (interactive) (load-file "~/.emacs.d/init.el"))
(global-set-key [f5] 'reload-init)

 
;; Scroll zoom
(global-set-key [C-mouse-4] 'text-scale-increase)
(global-set-key [C-mouse-5] 'text-scale-decrease)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#181512" "#8c644c" "#c4be90" "#fafac0" "#646a6d" "#6d6871" "#3b484a" "#bea492"])
 '(ansi-term-color-vector
   [unspecified "#181512" "#8c644c" "#c4be90" "#fafac0" "#646a6d" "#6d6871" "#646a6d" "#bea492"])
 '(custom-safe-themes
   (quote
    ("bb6b64bfb2f63efed8dea1ca03691c07c851a8be6f21675fe4909289d68975d9" "27eb4bbd908683d344af2a0b90d71698938ab9af1656b1aed87e68258ef8c980" "dc758223066a28f3c6ef6c42c9136bf4c913ec6d3b710794252dc072a3b92b14" "987b709680284a5858d5fe7e4e428463a20dfabe0a6f2a6146b3b8c7c529f08b" "1177fe4645eb8db34ee151ce45518e47cc4595c3e72c55dc07df03ab353ad132" "0b2e94037dbb1ff45cc3cd89a07901eeed93849524b574fa8daa79901b2bfdcf" "c0dd134ecd6ede6508c30f7d4ac92334229531df62284fc6572f65b4d0cde43f" "f2f2941e226bc578fa82b8badbb6ff252eef6b50b6f8f6263f8102cf5e029db8" "e3a3b7d7fe89b5d57d40bc825ca2324875a6f37bd63da66f2a6fc68cc8b2ee95" "7bf64a1839bf4dbc61395bd034c21204f652185d17084761a648251041b70233" "f641bdb1b534a06baa5e05ffdb5039fb265fde2764fbfd9a90b0d23b75f3936b" default)))
 '(fringe-mode 6 nil (fringe))
 '(linum-format " %7d ")
 '(main-line-color1 "#191919")
 '(main-line-color2 "#111111")
 '(powerline-color1 "#191919")
 '(powerline-color2 "#111111"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

					;(load-theme 'blackboard t)
(load-theme 'github t)
					;(load-theme 'tommyh t)
;(load-theme 'ritchie t)
					;(load-theme 'color-them-mac-classic t)
;(load-file "~/.emacs.d/themes/color-theme-mac-classic.el")
					;(color-theme-mac-classic)
					;(load-theme 'radiance t)
;(load-theme 'tron t)
					;(load-theme 'deep-thought t)
					;(load-theme 'assemblage t)
					;(load-theme 'professional t)
;(load-theme 'erosiond t)

