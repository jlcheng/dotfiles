;;; Installation --- 
;;;   echo '(load-file (expand-file-name "~/github/dotfiles/site-start.el"))' >> ~/.emacs

;;; http://orgmode.org/org.html
(cond ((file-accessible-directory-p "/cygdrive")
       (message "Windows OS")
       (setq org-agenda-files (list "~/org/home.org"
				    "~/privprjs/grs/docs/plan.org")))
      ((file-accessible-directory-p "/Users")
       (message "MacOS")
       (setq org-agenda-files (list "~/org/work.org"
				    "~/privprjs/grs/docs/plan.org"))))
(org-mode)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;;; emacsclient support
(server-start)
(ido-mode)

;;; https://shreevatsa.wordpress.com/2007/01/06/using-emacsclient/
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

;;; http://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files 
(setq make-backup-files nil)

;;; misc
(setq column-number-mode t)

;;; js-beautify
(defun jc-js-beautify (p1 p2)
  "Runs js-beautify, assumes installation path."
  (interactive "r")
  (shell-command-on-region
   p1 p2 "/usr/local/share/python/js-beautify -i -s 2" nil t))

