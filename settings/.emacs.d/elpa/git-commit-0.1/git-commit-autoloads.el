;;; git-commit-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "git-commit" "git-commit.el" (21785 40707 618371
;;;;;;  88000))
;;; Generated autoloads from git-commit.el

(autoload 'git-commit-mode "git-commit" "\
Major mode for editing git commit messages.
This mode helps with editing git commit messages both by
providing commands to do common tasks, and by highlighting the
basic structure of and errors in git commit messages.

Commands:\\<git-commit-map>
\\[git-commit-commit]   git-commit-commit  Finish editing and commit
\\[git-commit-signoff]   git-commit-signoff   Insert a Signed-off-by header
\\[git-commit-ack]   git-commit-ack   Insert an Acked-by header
\\[git-commit-test]   git-commit-test   Insert a Tested-by header
\\[git-commit-review]   git-commit-review   Insert a Reviewed-by header
\\[git-commit-cc]   git-commit-cc   Insert a Cc header
\\[git-commit-reported]   git-commit-reported   Insert a Reported-by header

Turning on git commit calls the hooks in `git-commit-mode-hook'.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG" . git-commit-mode))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; git-commit-autoloads.el ends here
