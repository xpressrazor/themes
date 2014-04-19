(require 'cc-mode)

(defun autopair-insert-opening ()
     (interactive)
     (when (autopair-pair-p)
       (setq autopair-action (list 'opening (autopair-find-pair) (point))))
     (autopair-fallback))

(setq-default c-basic-offset 4 c-default-style "linux")
(setq-default tab-width 4 indent-tabs-mode t)
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)

(add-to-list 'load-path "~/.emacs.d/")

(require 'autopair)
(autopair-global-mode 1)
(setq autopair-autowrap t)


(require 'auto-complete-config)
;(add-to-list 'ac-dictionary-directories "~/.emacs.d//ac-dict")
(ac-config-default)

(global-set-key  [f2] (lambda () (interactive) (manual-entry (current-word))))
(require 'compile)
 (add-hook 'c-mode-hook
           (lambda ()
	     (unless (file-exists-p "Makefile")
	       (set (make-local-variable 'compile-command)
                    ;; emulate make's .c.o implicit pattern rule, but with
                    ;; different defaults for the CC, CPPFLAGS, and CFLAGS
                    ;; variables:
                    ;; $(CC) -c -o $@ $(CPPFLAGS) $(CFLAGS) $<
		    (let ((file (file-name-nondirectory buffer-file-name)))
                      (format "%s -c -o %s.o %s %s %s"
                              (or (getenv "CC") "gcc")
                              (file-name-sans-extension file)
                              (or (getenv "CPPFLAGS") "-DDEBUG=9")
                              (or (getenv "CFLAGS") "-ansi -pedantic -Wall -g")
			      file))))))


(defun recompile-quietly ()
  "Re-compile without changing the window configuration."
  (interactive)
  (save-window-excursion
    (recompile)))

;; Compile and close buffer if no error
;; Helper for compilation. Close the compilation window if
;; there was no error at all. (emacs wiki)
(defun compilation-exit-autoclose (status code msg)
  ;; If M-x compile exists with a 0
  (when (and (eq status 'exit) (zerop code))
    ;; then bury the *compilation* buffer, so that C-x b doesn't go there
    (bury-buffer)
    ;; and delete the *compilation* window
    (delete-window (get-buffer-window (get-buffer "*compilation*"))))
  ;; Always return the anticipated result of compilation-exit-message-function
  (cons msg code))
;; Specify my function (maybe I should have done a lambda function)
(setq compilation-exit-message-function 'compilation-exit-autoclose)


;;; Emacs is not a package manager, and here we load its package manager!

(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(menu-bar-mode -1)
(show-paren-mode t)
(setq show-paren-delay 0)
(global-set-key [f9] 'toggle-menu-bar-mode-from-frame)

;; "y or n" instead of "yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;; Display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Prevent the annoying beep on errors
;(setq visible-bell t)

;; Line-wrapping
(set-default 'indicate-empty-lines t)

;; Line-wrapping
;(set-default 'fill-column 80)

;; Make sure all backup files only live in one place
;(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Don't truncate lines
;(setq truncate-lines t)

;; Trailing white space is unnecessary
; (add-hook 'before-save-hook (lamda() (delete-trailing-whitespace)))

;(global-set-key [(control c) (c)] 'compile-again)

(setq compilation-last-buffer nil)
(defun compile-again (pfx)
  """Run the same compile as the last time.

If there was no last time, or there is a prefix argument, this acts like
M-x compile.
"""
 (interactive "p")
 (if (and (eq pfx 1)
	  compilation-last-buffer)
     (progn
       (set-buffer compilation-last-buffer)
       (revert-buffer t t))
   (call-interactively 'compile)))


 (defun quick-copy-line ()
      "Copy the whole line that point is on and move to the beginning of the next line.
    Consecutive calls to this command append each line to the
    kill-ring."
      (interactive)
      (let ((beg (line-beginning-position 1))
            (end (line-beginning-position 2)))
        (if (eq last-command 'quick-copy-line)
            (kill-append (buffer-substring beg end) (< end beg))
          (kill-new (buffer-substring beg end))))
      (beginning-of-line 2))

(global-set-key "\C-c\C-j" 'quick-copy-line)

; Functions
(global-set-key [f6] 'buffer-menu)
(global-set-key [f7] 'other-window)
(global-set-key [f11] 'compile)
(global-set-key "\C-x\C-m" 'compile)
(global-set-key (kbd "TAB") 'smart-tab)
(global-set-key "\C-x\C-a" 'compile-again)

; Easy keys to split window
(global-set-key (kbd "M-1") 'delete-other-windows) ; expand pane
(global-set-key (kbd "M-2") 'split-window-vertically)
(global-set-key (kbd "M-3") 'split-window-horizontally)
(global-set-key (kbd "M-0") 'delete-window) ; current
(global-set-key (kbd "M-4") 'other-window) ; switchg
(global-set-key (kbd "M-o") 'other-window) ; switchg
(global-set-key (kbd "M-5") 'find-file)
(global-set-key (kbd "M-6") 'switch-to-prev-buffer)
(global-set-key (kbd "M-7") 'switch-to-next-buffer)
(global-set-key (kbd "M-8") 'save-buffer)
(global-set-key (kbd "C-u") 'save-buffer)
(global-set-key (kbd "C-q") 'save-buffers-kill-emacs)

;; Mark all
(global-set-key (kbd "M-9") 'mark-whole-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-k") 'kill-this-buffer)
;(global-set-key (kbd "TAB") 'smart-tab)
