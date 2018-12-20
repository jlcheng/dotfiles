;;; Installation --- 
;;;   echo '(load-file (expand-file-name "~/privprjs/dotfiles/site-start.el"))' >> ~/.emacs.d/init.el

;; depends on following in ~/.emacs.d/init.el
; (require 'package)
; (add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/"))
; (package-initialize)


(defun jc-org-cygwin ()
  "Windows specific org-mode customizations"
  (message (documentation 'jc-org-cygwin))
  (setq org-agenda-files (list "~/org/home.org"
                                    "~/privprjs/grs/docs/plan.org")))

(defun jc-org-macos ()
  "macOS specific org-mode customizations"
  (message (documentation 'jc-org-macos))
  (setq org-agenda-files (list "~/org/work.org"
                               "~/privprjs/grs/docs/plan.org"
			       "~/org/work/work_journal.org"))
  )

(defun jc-misc-macos()
  "macos misc customizations"
  (message (documentation 'jc-misc-macos))
  (setq mac-command-modifier 'meta) ;; so the Alt key on WASD Code can be used for 'M-x'
  (setq mac-option-modifier 'super) ;; so the key left of Alt on WAS Code can be used for 'S-p'
  (global-unset-key (kbd "s-w")) ;; macOS: frequenly leads to accidental killing frames
  (global-unset-key (kbd "s-n")) ;; macOS: frequenly leads to accidental new frames
  )

;;; OS and env-specific settings
(cond ((file-accessible-directory-p "/cygdrive")
       (message "Windows OS")
       (jc-org-cygwin)
       )
      ((eq system-type 'darwin)
       (message "macOS")
       (jc-org-macos)
       (jc-misc-macos)
       )
      )

(org-mode)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-switchb)
;; 2018-10-15 unclutter directories with org files
(setq org-archive-location "~/org/archive/archive.org::* From %s")
;; 2018-11-07 experimenting with turning on auto-fill-mode for org-mode
(add-hook 'org-mode-hook 'auto-fill-mode)
(add-hook 'org-mode-hook (lambda()
			   (set-fill-column 120)))
(setq org-startup-folded nil) ;; https://orgmode.org/manual/Initial-visibility.html#Initial-visibility
(setq org-startup-indented t) ;; https://orgmode.org/manual/Clean-view.html

(global-set-key (kbd "M-s M-s") 'save-buffer) ;; left hand saver; my left pinky is killing me from hitting ctrl all the time.
(global-set-key (kbd "M-n M-j r") 'revert-buffer)
(global-set-key (kbd "M-n M-j b") 'jsnice-jc)
(global-set-key (kbd "M-n M-j s") 'whitespace-mode)
(global-set-key (kbd "M-n M-j o") 'org-sort-jc)
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

;; 2018-12-24: Experiment with helm-mode for completion
(cond
 ((functionp 'helm-mode) (helm-mode))
 ((functionp 'ivy-mode) (ivy-mode)))
  
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


;; Shortcut to frequently used files, can be used to replace projectile
(defvar freq-files-def-jc '("~" "~/privprjs/dotfiles/site-start.el" "~/org/home.org")
  "Frequently used files")
(defun sc-jc ()
  "Shortcut to frequently used files"
  (interactive)
  (find-file-existing
   (let ((crf (cond ((functionp 'helm-comp-read)
		     'helm-comp-read)
		    ((functionp 'ivy-completing-read)
		     'ivy-completing-read)
		    ((functionp 'ido-completing-read)
		     'ido-completing-read))))
     (funcall crf "freq-files-jc: " freq-files-def-jc))
   ))
(global-set-key (kbd "M-n M-j p") 'sc-jc)

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(if (functionp 'global-company-mode) (global-company-mode))

;; 2018-10-29: Sets full file path in title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; 2018-12-06: Has not used this since it was written, though sounds useful.
(defun eval-to-kill-ring-jc ()
  (interactive)
  (kill-new (with-output-to-string (princ (call-interactively 'eval-expression)))))
(global-set-key (kbd "M-n M-j M-:") 'eval-to-kill-ring-jc)

(defun kill-new-file-name ()
  (interactive)
  (kill-new (format "[[%s]]" (buffer-file-name))))
(global-set-key (kbd "s-k") 'kill-new-file-name)

;;; --- tramp mode ---
;; note from 2018-10-25, if editing over ssh is slow, try setting this
;; https://www.emacswiki.org/emacs/TrampMode 
;; (setq tramp-default-method "ssh")

;;; --- projectile ---
;; experiment from 2018-12-05
;; M-x project-refresh-contents
;; M-x project-install projectile
;; https://projectile.readthedocs.io/en/latest/usage/
;(projectile-mode +1)
;(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
;(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
;(setq projectile-indexing-method 'alien)
;(setq projectile-completion-system 'ivy)
;(setq projectile-project-search-path '("~/org/"))
;(setq projectile-ignored-projects ["~/go/src/go.zr.org/"])
;(setq projectile-globally-ignored-file-suffixes ["org_archive"])
;; Removed on 2018-12-23, replaced with sc-jc

;;; -- font size --
(set-face-attribute 'default (selected-frame) :height 150)


;;; -- start in Messages buffer
(setq inhibit-startup-screen t)

