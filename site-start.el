;;; Installation --- 
;;;   echo '(load-file (expand-file-name "~/github/dotfiles/site-start.el"))' >> ~/.emacs

;;; http://orgmode.org/org.html
(cond ((file-accessible-directory-p "/cygdrive")
       (message "Windows OS")
       (setq org-agenda-files (list "~/org/home.org"
				    "~/privprjs/grs/docs/plan.org")))
      ((file-accessible-directory-p "/rubicon")
       (message "MacOS-RP")
       (setq org-agenda-files (list "~/org/work.org")))
      ((file-accessible-directory-p "/Users")
       (message "MacOS")
       (setq org-agenda-files (list "~/org/work.org"
				    "~/privprjs/grs/docs/plan.org"))))
(org-mode)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key (kbd "M-n M-j r") 'revert-buffer)
(global-set-key (kbd "M-n M-j b") 'jc-js-beautify)

;;; enable emacsclient support unless we're running 'emacs-nox'
; note: string-match-p not avail on Emacs 22.1.1 on MacOS (latest release is 25.3 as of Sept 2017)
(unless
    (string-match (regexp-quote "emacs-nox") (elt command-line-args 0))
  (server-start))
(ido-mode)

;;; https://shreevatsa.wordpress.com/2007/01/06/using-emacsclient/
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

;;; http://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files 
(setq make-backup-files nil)

;;; https://www.emacswiki.org/emacs/BackupDirectory#toc2 - without the fancy ',temporary-file-directory' evaluation
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;;; misc
(setq column-number-mode t)

;;; js-beautify
(defun jc-js-beautify (p1 p2)
  "Runs js-beautify against the region"
  (interactive "r")
  (if (executable-find "js-beautify")
      (shell-command-on-region
       p1 p2 "js-beautify -i -s 2" nil t)
    (message "js-beautify not installed")))

