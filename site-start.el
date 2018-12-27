;; site-start.el --- personalization -*- lexical-binding: t -*-
;;; Installation --- 
;;;   echo '(load-file (expand-file-name "~/privprjs/dotfiles/site-start.el"))' >> ~/.emacs.d/init.el

;; Shortcut to frequently used files, can be used to replace projectile
(defvar freq-files-def-jc '("~/privprjs/dotfiles/site-start.el" "~/org/home.org")
  "Frequently used files. Initially populated from ~/.sc-jc.txt")
(let ((file "~/.sc-jc.txt"))
  (if (file-exists-p file)
      (let* ((flist (with-temp-buffer
		      (insert-file-contents file)
		      (split-string (buffer-string) "\n" t)))
	     (flist (seq-filter 'file-exists-p flist)))
	(setq freq-files-def-jc (cl-remove-duplicates (append freq-files-def-jc flist) :test #'equal)))))
(defun sc-jc ()
  "Shortcut to frequently used files"
  (interactive)
  (find-file-existing
   (let ((crf (cl-first (seq-filter 'functionp '(helm-comp-read ivy-completing-read ido-completing-read)))))
     (funcall crf "freq-files-jc: " freq-files-def-jc))
   )
  )
(global-set-key (kbd "M-n M-j p") 'sc-jc)

(defun org-cygwin-jc ()
  "Windows specific org-mode customizations"
  (message (documentation 'org-cygwin-jc))
  (setq org-agenda-files '("~/org/home.org"
			   "~/privprjs/grs/docs/plan.org")))

(defun org-macOS-jc ()
  "macOS specific org-mode customizations"
  (message (documentation 'org-macOS-jc))
  (setq org-agenda-files '("~/org/home.org" "~/org/work/work_journal.org")))


(defun misc-macOS-jc ()
  "macOS misc customizations"
  (message (documentation 'misc-macOS-jc))
  (setq mac-command-modifier 'meta) ;; so the Alt key on WASD Code can be used for 'M-x'
  (setq mac-option-modifier 'super) ;; so the key left of Alt on WAS Code can be used for 'S-p'
  (global-unset-key (kbd "s-w")) ;; macOS: frequenly leads to accidental killing frames
  (global-unset-key (kbd "s-n")) ;; macOS: frequenly leads to accidental new frames
  )

(defun org-linux-jc ()
  "gnu/linux specific org-mode customizations"
  (message (documentation 'org-linux-jc))
  (setq org-agenda-files '("~/org/home.org"
			   "~/privprjs/grs/docs/plan.org"))
  )

;;; OS and env-specific settings
(cond ((eq system-type 'cygwin)
       (message "Windows OS")
       (org-cygwin-jc)
       (set-face-attribute 'default (selected-frame) :height 130)
       )
      ((eq system-type 'darwin)
       (message "macOS")
       (org-macOS-jc)
       (misc-macOS-jc)
       (setq default-frame-alist '((top . 0) (left . 0) (height . 60) (width . 160)))
       (add-to-list 'freq-files-def-jc "~/org/work/work_journal.org")       
       (set-face-attribute 'default (selected-frame) :height 130)
       )
      ((eq system-type 'gnu/linux)
       (message "gnu/linux")
       (set-face-attribute 'default (selected-frame) :height 135)
       (org-linux-jc))
      )



(org-mode)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-switchb)
;; 2018-10-15 unclutter directories with org files
(setq org-archive-location "~/org/archive/archive.org::* From %s")
;; 2018-11-07 experimenting with turning on auto-fill-mode for org-mode
(defun org-mode-hook-jc ()
  "org-mode hooks"
  (auto-fill-mode)
  (set-fill-column 120))
(add-hook 'org-mode-hook 'org-mode-hook-jc)
(setq org-startup-folded nil  ;; https://orgmode.org/manual/Initial-visibility.html#Initial-visibility
      org-startup-indented t) ;; https://orgmode.org/manual/Clean-view.html

(global-set-key (kbd "M-s M-s") 'save-buffer) ;; left hand saver; my left pinky is killing me from hitting ctrl all the time.
(global-set-key (kbd "M-n M-j r") 'revert-buffer)
(global-set-key (kbd "M-n M-j b") 'jsnice-jc)
(global-set-key (kbd "M-n M-j s") 'whitespace-mode)
(global-set-key (kbd "M-n M-j o") 'org-sort-jc)
;; 2018-10-29 starting to use imenu in org mode, creating a kbd shortcut for it
;; 2018-12-25 schedule use of imenu in org mode for removal; rarely used
;; (global-set-key (kbd "M-n M-j i") 'imenu)

;;; enable emacsclient support unless we're running 'emacs-nox'
; note: string-match-p not avail on Emacs 22.1.1 on MacOS (latest release is 25.3 as of Sept 2017)
(unless
    (string-match (regexp-quote "emacs-nox") (elt command-line-args 0))
  ;; runs emacs server
  (server-start)
  ;; Ubuntu: run a no-op command to bring the window into focus
  (add-hook 'server-visit-hook (lambda() (message " "))))

;; 2018-12-24: Experiment with helm-mode for completion
(unless (package-installed-p 'helm)
  (progn
    (add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/"))
    (package-refresh-contents)
    (package-install 'helm)))
(let ((modes '(helm-mode ivy-mode)))
  (funcall (cl-first (seq-filter 'functionp modes))))
  
;;; https://shreevatsa.wordpress.com/2007/01/06/using-emacsclient/
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

;; https://www.emacswiki.org/emacs/BackupDirectory
(setq
 backup-by-copying t
 backup-directory-alist '(("." . "/home/jcheng/.emacs.d/backups"))
 delete-old-versions t
 kept-new-versions 5
 kept-old-versions 3
 version-control t)
(setq create-lockfiles nil) ;; do not create '.#lock' files


;;; misc
(setq column-number-mode t)

(defun jsnice-jc (p1 p2)
  "Runs jsnice against the region"
  (interactive "r")
  (let ((jsnice-path (expand-file-name "~/bin/jsnice")))
    (cond ((eq system-type 'cygwin)
	   (setq jsnice-path "C:\\cygwin64\\home\\johnl\\bin\\jsnice.exe")))
    (if (executable-find jsnice-path)
	(shell-command-on-region
	 p1 p2 jsnice-path nil t "*Minibuf-0*" t)
      (message (format "%s not installed" jsnice-path)))))

(defun notabs-jc ()
  "Runs untabify against the buffer"
  (untabify (point-min) (point-max)))

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin")) ;; Needed for M-x shell-command
(add-to-list 'exec-path "/usr/local/bin")                  ;; Needed for (executable-find ...)
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

(defun kill-new-file-name (b)
  "Append the path of an open file into the kill ring"
  (interactive "b")
  (let ((bfn (buffer-file-name (get-buffer b))))
    (if bfn
	(progn
	  (kill-new bfn)
	  (message bfn)))
    )
  )
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

;;; -- start in *scratch* buffer
(setq inhibit-startup-screen t)

;;; -- spellcheck
(cond ((executable-find "aspell")
       (setq ispell-program-name "aspell")
       (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))))
