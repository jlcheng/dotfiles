;; site-start.el --- personalization -*- lexical-binding: t -*-
;;; Installation --- 
;;;   echo '(load-file (expand-file-name "~/privprjs/dotfiles/site-start.el"))' >> ~/.emacs.d/init.el

(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/"))

;; library functions
(defun jc/file-readlines (file)
  "Returns contents of file as list of strings"
  (if (file-exists-p file)
      (with-temp-buffer
	(insert-file-contents file)
	(split-string (buffer-string) "\n" t))
    ))

;; Shortcut to frequently used files, can be used to replace projectile
(defvar jc/freq-files-def '("~/privprjs/dotfiles/site-start.el" "~/org/home.org")
  "Frequently used files. Initially populated from ~/.sc-jc.txt")
(let ((file "~/.sc-jc.txt"))
  (if (file-exists-p file)
      (let ((flist (jc/file-readlines file)))          ; read sc-jc.txt
        (setq flist (seq-filter 'file-exists-p flist)) ; check for valid files
	(setq flist (append jc/freq-files-def flist))  ; merge sc-jc.txt with defaults
	(setq jc/freq-files-def (cl-remove-duplicates flist :test #'equal)))))

(defun jc/shortcuts ()
  "Shortcut to frequently used files"
  (interactive)
  (let ((crf)
	(crf-list nil)
	(target nil))
    (setq crf-list '(helm-comp-read
		     ivy-completing-read
		     ido-completing-read
		     completing-read)
	  crf (seq-find 'functionp crf-list)
	  target (funcall crf "freq-files-jc: " jc/freq-files-def))
    (find-file-existing target)
    ))
(global-set-key (kbd "M-n M-p") 'jc/shortcuts)

(defun misc-macOS-jc ()
  "macOS misc customizations"
  (message (documentation 'misc-macOS-jc))
  (setq mac-command-modifier 'meta) ;; so the Alt key on WASD Code can be used for 'M-x'
  (setq mac-option-modifier 'super) ;; so the key left of Alt on WAS Code can be used for 'S-p'
  (global-unset-key (kbd "s-w"))    ;; macOS: frequenly leads to accidental killing frames
  (global-unset-key (kbd "s-n")))   ;; macOS: frequenly leads to accidental new frames

;; OS-specific settings:
;;  - misc
;;  - GUI customizations
(cl-case system-type
  ((cygwin)
   (message "Windows OS")
   (setq default-frame-alist '((top . 0) (left . 0) (height . 39) (width . 132)))
   (set-face-attribute 'default (selected-frame) :height 130))
  ((darwin)
   (message "macOS")
   (misc-macOS-jc)
   (setq default-frame-alist '((top . 0) (left . 0) (height . 60) (width . 160))))
  ((gnu/linux)
   (message "gnu/linux")
   (setq default-frame-alist '((top . 0) (left . 0) (height . 39) (width . 132)))
   (set-face-attribute 'default (selected-frame) :height 135)))

;; === START: org-mode ===
(org-mode)
(defun org-mode-hook-jc ()
  "org-mode hooks. auto-fill has been useful."
  (auto-fill-mode)
  (set-fill-column 120))
(defun org-last-heading-same-level-jc ()
  "move to last heading on the same level"
  (interactive)
  (org-forward-heading-same-level 1000)
  (org-next-visible-heading 1)
  (backward-char))

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-switchb)
(global-set-key (kbd "M-n M-i") 'helm-semantic-or-imenu)
(global-set-key (kbd "M-n M-c M-f") 'org-last-heading-same-level-jc)

(setq org-archive-location "~/org/archive/archive.org::* From %s"
      org-startup-folded nil  ;; https://orgmode.org/manual/Initial-visibility.html#Initial-visibility
      org-startup-indented t  ;; https://orgmode.org/manual/Clean-view.html
      
      ) ; unclutter directories with org files

;; List of files to add to org-agenda-files
(setq org-agenda-files '("~/org/home.org"))
(let ((file "~/.org-jc.txt"))
  (if (file-exists-p file)
      (let ((flist (jc/file-readlines file)))          ; read org-jc.txt
        (setq flist (seq-filter 'file-exists-p flist)) ; check for valid files
	(setq flist (append org-agenda-files flist))   ; merge org-jc.txt with defaults
	(setq org-agenda-files (cl-remove-duplicates flist :test #'equal)))))

(add-hook 'org-mode-hook 'org-mode-hook-jc) 
;; === STOP: org-mode ===
(setq imenu-auto-rescan t)

(global-set-key (kbd "M-s M-s") 'save-buffer) ;; left hand saver; my left pinky is killing me from hitting ctrl all the time.
(global-set-key (kbd "M-n M-r") 'revert-buffer)
(global-set-key (kbd "M-n M-b") 'jsnice-jc)
(global-set-key (kbd "M-n M-s") 'whitespace-mode)
(global-set-key (kbd "M-n M-1") 'point-to-register)
(global-set-key (kbd "M-n M-2") 'jump-to-register)

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
  (package-refresh-contents)
  (package-install 'flyspell-correct-helm)
  (package-install 'helm)       ;; helm is a super nice completion system
  (package-install 'helm-rg))  ;; install 'ripgrep' to use this
(global-set-key (kbd "M-n M-f") 'helm-rg)
(let ((modes '(helm-mode ivy-mode)))
  (funcall (cl-first (seq-filter 'functionp modes))))
(when (functionp 'helm-mode)
  (global-set-key (kbd "C-x C-b") 'helm-mini)
  (global-set-key (kbd "C-x C-f") 'helm-find-files))

;;; https://shreevatsa.wordpress.com/2007/01/06/using-emacsclient/
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

;; https://www.emacswiki.org/emacs/BackupDirectory
(setq
 backup-by-copying t
 backup-directory-alist `(("." . ,(expand-file-name "~/.emacs.d/backups"))) ;; influences (make-backup-file-name "~/tmp.txt")
 auto-save-file-name-transforms `((".*" ,temporary-file-directory t))       ;; influences (make-auto-save-file-name), e.g., #foo.txt#
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

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin")) ;; Needed for M-x shell-command
(add-to-list 'exec-path "/usr/local/bin")                  ;; Needed for (executable-find ...)
(if (functionp 'global-company-mode) (global-company-mode))

;; 2018-10-29: Sets full file path in title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(defun jc/kill-new-file-name (b)
  "Append the path of an open file into the kill ring"
  (interactive "b")
  (let ((bfn (buffer-file-name (get-buffer b))))
    (when bfn
      (kill-new bfn)
      (message bfn))))
(global-set-key (kbd "M-n M-k") 'jc/kill-new-file-name)

;;; -- start in *scratch* buffer
(setq inhibit-startup-screen t)

;;; -- spellcheck
(require 'flyspell-correct-helm)
(global-set-key (kbd "M-$") 'flyspell-correct-at-point)
(when (executable-find "aspell")
  (setq ispell-program-name "aspell")
  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")))

;; https://www.emacswiki.org/emacs/RecentFiles#toc9 - prevents blocking of emacs startup
(require 'recentf)
(setq recentf-auto-cleanup 'never)
(recentf-mode 1)

