;;; Installation --- 
;;;   echo '(load-file (expand-file-name "~/privprjs/dotfiles/site-start.el"))' >> ~/.emacs.d/init.el

;; depends on following in ~/.emacs.d/init.el
; (require 'package)
; (add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/"))
; (package-initialize)


;;; http://orgmode.org/org.html
(cond ((file-accessible-directory-p "/cygdrive")
       (message "Windows OS")
       (setq org-agenda-files (list "~/org/home.org"
                                    "~/privprjs/grs/docs/plan.org")))
      ((file-accessible-directory-p "/Users")
       (message "MacOS")
       (setq org-agenda-files (list "~/org/work.org"
                                    "~/privprjs/grs/docs/plan.org"))
       ;;(setq mac-command-modifier 'meta) changed this to mac-option-modifier 'meta so that Emacs on macOS is consistent with JetBrain
       (setq mac-option-modifier 'meta)
       ))


(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super))

(org-mode)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-switchb)
;; 2018-10-15 unclutter directories with org files
(setq org-archive-location "~/org/archive/archive.org::* From %s")
;; 2018-11-07 experimenting with turning on auto-fill-mode for org-mode
(add-hook 'org-mode-hook 'auto-fill-mode)
(setq org-startup-folded nil) ;; https://orgmode.org/manual/Initial-visibility.html#Initial-visibility
(setq org-startup-indented t)


(global-set-key (kbd "M-s M-s") 'save-buffer) ;; left hand saver; my left pinky is killing me from hitting ctrl all the time.
(global-set-key (kbd "M-n M-j r") 'revert-buffer)
(global-set-key (kbd "M-n M-j b") 'jsnice-jc)
(global-set-key (kbd "M-n M-j s") 'whitespace-mode)
(global-set-key (kbd "M-n M-j o") 'org-sort-jc)
(global-unset-key (kbd "s-w")) ;; macOS: frequenly leads to accidental killing frames
(global-unset-key (kbd "s-n")) ;; macOS: frequenly leads to accidental new frames
;; 2018-10-29 starting to use imenu in org mode, creating a kbd shortcut for it
(global-set-key (kbd "M-n M-j i") 'imenu)

;;; enable emacsclient support unless we're running 'emacs-nox'
; note: string-match-p not avail on Emacs 22.1.1 on MacOS (latest release is 25.3 as of Sept 2017)
(unless
    (string-match (regexp-quote "emacs-nox") (elt command-line-args 0))
  ;; runs emacs server
  (server-start)
  ;; Uubuntu: run a no-op command to bring the window into focus
  (add-hook 'server-visit-hook (lambda() (message " "))))
(ivy-mode) ; trying this out instead of (ido-mode)

;;; https://shreevatsa.wordpress.com/2007/01/06/using-emacsclient/
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

;;; http://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files 
(setq make-backup-files nil)

;;; https://www.emacswiki.org/emacs/BackupDirectory#toc2 - without the fancy ',temporary-file-directory' evaluation
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;;; misc
(setq column-number-mode t)

;;; jsnice
(setq jsnice-path (expand-file-name "~/bin/jsnice"))
(cond ((file-accessible-directory-p "/cygdrive")
       (message "Windows OS")
       (setq jsnice-path "C:\\cygwin64\\home\\johnl\\bin\\jsnice.exe")))
(defun jsnice-jc (p1 p2)
  "Runs jsnice against the region"
  (interactive "r")
  (if (executable-find jsnice-path)
      (shell-command-on-region
       p1 p2 jsnice-path nil t "*Minibuf-0*" t)
    (message (format "%s not installed" jsnice-path))))

(defun notabs-jc ()
  "Runs untabify against the buffer"
  (interactive)
  (untabify (point-min) (point-max)))

;; 2018-11-07: When is the last time I used this?
(defun org-sort-jc ()
  "Runs org-sort agianst the buffer"
  (interactive)
  (save-excursion
    ;; mark region
    (goto-char (point-max))
    (push-mark)
    (goto-char 0)
    (org-sort-entries t ?p)
    )
  )

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(global-company-mode)

;; 2018-10-29: Sets full file path in title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;;; --- tramp mode ---
;; note from 2018-10-25, if editing over ssh is slow, try setting this
;; https://www.emacswiki.org/emacs/TrampMode 
;; (setq tramp-default-method "ssh")

