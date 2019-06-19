;; site-start.el --- personalization -*- lexical-binding: t -*-
;;; Installation --- 
;;;   echo '(load-file (expand-file-name "~/privprjs/dotfiles/site-start.el"))' >> ~/.emacs.d/init.el

;; Required packages
(defvar jc/package-refreshed-p nil "Sets to t once we ran package-refresh-contents once")
(defun jc/package-refresh-contents-once ()
  "Runs package-refresh-contents once per session"
  (unless jc/package-refreshed-p
    (package-refresh-contents)
    (setq jc/package-refreshed-p t)))    
(package-initialize)
(add-to-list 'package-archives (cons "melpa" "http://melpa.org/packages/"))
(setq package-archives '(("melpa" . "http://melpa.org/packages/")))
(defun jc/init/installs ()
  "Installs favorite packages"
  (unless (package-installed-p 'helm) ;; helm is a super nice completion system
    (jc/package-refresh-contents-once)
    (package-install 'flyspell-correct-helm)
    (package-install 'helm)      
    (package-install 'helm-rg))  ;; install 'ripgrep' to use this
  (unless (package-installed-p 'origami)
    (jc/package-refresh-contents-once)
    (package-install 'origami))
  (unless (package-installed-p 'json-navigator) ;; 2019-04-16: try json-navigator - tab, shift-tab, enter
    (jc/package-refresh-contents-once)
    (package-install 'json-navigator))
  (unless (package-installed-p 'projectile) ;; 2019-05-18 try projectile; 2019-05-22 (liking it)
    (jc/package-refresh-contents-once)
    (package-install 'projectile)
    (package-install 'helm-projectile))
  (unless (package-installed-p 'go-playground) ;; 2019-05-27 try go-playground
    (jc/package-refresh-contents-once)
    (package-install 'go-playground))
  (unless (package-installed-p 'which-key) ;; 2019-05-27 try go-playground
    (jc/package-refresh-contents-once)
    (package-install 'which-key))
  (unless (package-installed-p 'graphviz-dot-mode)
    (jc/package-refresh-contents-once)
    (package-install 'graphviz-dot-mode))
  (unless (package-installed-p 'flyspell-correct-helm)
    (package-install 'flyspell-correct-helm)))
(jc/init/installs)

;; library functions
(defun jc/file-readlines (file)
  "Returns contents of file as list of strings"
  (if (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (split-string (buffer-string) "\n" t))
    ))

;; Keymaps
(defvar jc/right-map (make-keymap) "Keys whose suffix are intended for the right hand.")
(define-key global-map (kbd "M-n") jc/right-map)
(defvar jc/left-map (make-keymap) "Keys whose suffix are intended for the left hand.")
(define-key global-map (kbd "M-c") jc/left-map)
(defvar jc/c-1-map (make-keymap) "Keys bound to C-1. Shortcuts for misc.")
(define-key global-map (kbd "C-1") jc/c-1-map)
(defun jc/show-keymaps ()
  "Shows my personalized keymaps"
  (interactive)
  (let ((jc/keymaps [jc/right-map jc/left-map jc/c-1-map]))
    (with-output-to-temp-buffer "*jc/keymaps help*"
      (seq-map (lambda (elt)
	       (let ((p))
		 (princ (format "=== %s ===\n" elt))
		 (setq p (substitute-command-keys (format "\\{%s}" elt)))
		 (setq p (replace-regexp-in-string "^.+Prefix Command$" "" p))
		 (setq p (replace-regexp-in-string "\n*\n" "\n" p))
		 (princ p)
		 ))
             jc/keymaps))))
(define-key jc/right-map (kbd "M-n M-k") 'jc/show-keymaps)

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
(defun jc/org-mode-hook ()
  "org-mode hooks. auto-fill has been useful."
  (set-fill-column 120)
  (define-key org-mode-map (kbd "C-c C-0") 'org-mark-ring-goto))
(add-hook 'org-mode-hook 'jc/org-mode-hook)

(defun org-last-heading-same-level-jc ()
  "move to last heading on the same level"
  (interactive)
  (org-forward-heading-same-level 1000)
  (org-next-visible-heading 1)
  (backward-char))
(define-key jc/left-map (kbd "M-f") 'org-last-heading-same-level-jc)
(global-set-key (kbd "C-c a") 'org-agenda)
(define-key jc/left-map (kbd "M-b") 'org-switchb)

(setq org-archive-location "~/org/archive/%s_archive.org::datetree/* Finished Tasks"
      org-startup-folded 'content ;; https://orgmode.org/manual/Initial-visibility.html#Initial-visibility
      org-startup-indented t      ;; https://orgmode.org/manual/Clean-view.html
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      ) ; unclutter directories with org files

(defun jc/org-append-agenda-file (newfile)
  "append a file, if non-present, to ~/.org-jc.txt"
  (interactive)
  (let ((org-agendas-file "~/.org-jc.txt"))
    (if (file-exists-p org-agendas-file)
	(let ((flist (jc/file-readlines org-agendas-file)))          ; read org-jc.txt
	  (unless (seq-contains flist newfile)
	    (write-region newfile nil org-agendas-file 'append))
	  ))))
(defun jc/org-append-agenda-file-this ()
  "append this buffer to ~/.org-jc.txt"
  (interactive)
  (let ((bfn (buffer-file-name)))
    (if (file-exists-p buffer-file-name)
	(progn 
	  (jc/org-append-agenda-file bfn)
	  (jc/org-refresh-agenda-files)
	  (find-file "~/.org-jc.txt"))
      )
    )
  )
(setq org-agenda-files "~/.org-jc.txt")
  

(setq org-use-speed-commands t) ;; 2019-02-26 Trying this (use 'n' and 'p' to navigate up and down)
;; === STOP: org-mode ===

;; === START: origami-mode ===
(defun jc/origami-mode-init ()
  "Init-time customizations for origami-mode"
  (add-hook 'emacs-lisp-mode-hook 'origami-mode)
  (add-hook 'python-mode-hook 'origami-mode)
  (setq origami-show-fold-header t))
(jc/origami-mode-init)
;; === STOP: origami-mode ===

;; === START: go-mode ===
(defun jc/org-mode-init ()
  "Init-time customizations for org-mode"
  (setq go-test-args "-v")
  )
(jc/org-mode-init)
;; === STOP: go-mode ===

;; === START: python-mode ===
(setq python-shell-interpreter "python3")
;; === STOP: python-mode ===

(global-set-key (kbd "M-s M-s") 'save-buffer) ;; left hand saver; my left pinky is killing me from hitting ctrl all the time.
(define-key jc/right-map (kbd "M-j") 'jsnice-jc) ;; [j]son indent
(define-key jc/right-map (kbd "M-p") 'bookmark-jump)
(define-key jc/right-map (kbd "M-h") 'command-history)
(define-key jc/left-map (kbd "M-r") 'revert-buffer)
(define-key jc/left-map (kbd "M-s") 'whitespace-mode) ;; toggle [s]paces


;;; enable emacsclient support unless we're running 'emacs-nox'
(unless
    (string-match "emacs-nox" (cl-first command-line-args))
  ;; runs emacs server
  (server-start)
  ;; Ubuntu: run a no-op command to bring the window into focus
  (add-hook 'server-visit-hook (lambda() (message " "))))

(define-key jc/right-map (kbd "M-f") 'helm-rg)
(let ((modes '(helm-mode ivy-mode)))
  (funcall (cl-first (seq-filter 'functionp modes))))
(when (functionp 'helm-mode)
  (define-key global-map (kbd "C-x C-f") 'helm-find-files)
  (define-key global-map (kbd "C-x C-b") 'helm-mini)
  (define-key global-map (kbd "M-x") 'helm-M-x))

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
 version-control t
 create-lockfiles nil)

;;; misc
(setq column-number-mode t)
(setq imenu-auto-rescan t)
(setq confirm-kill-emacs 'y-or-n-p)
(setq register-preview-delay 0)
(setq scroll-error-top-bottom t)
(show-paren-mode 1)
(setq show-paren-delay 0)
(define-key jc/right-map (kbd "M-i") 'helm-semantic-or-imenu)
(define-key jc/left-map (kbd "t") 'origami-toggle-all-nodes)  ;; [t]oggle
(define-key jc/c-1-map (kbd "C-r") 'point-to-register) ;; [r]emember
(define-key jc/c-1-map (kbd "C-g") 'jump-to-register)  ;; [g]oto
(add-hook 'markdown-mode-hook '(lambda () (define-key markdown-mode-map (kbd "M-n") nil)))

(defun jsnice-jc (p1 p2)
  "Runs jsnice against the region"
  (interactive "r")
  (if (functionp 'json-pretty-print)
      (json-pretty-print p1 p2)
    (let ((jsnice-path (expand-file-name "~/bin/jsnice")))
      (cond ((eq system-type 'cygwin)
             (setq jsnice-path "C:\\cygwin64\\home\\johnl\\bin\\jsnice.exe")))
      (if (executable-find jsnice-path)
          (shell-command-on-region
           p1 p2 jsnice-path nil t "*Minibuf-0*" t)
	(message (format "%s not installed" jsnice-path))))))


(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin")) ;; Needed for M-x shell-command
(add-to-list 'exec-path "/usr/local/bin")                  ;; Needed for (executable-find ...)
(if (functionp 'global-company-mode) (global-company-mode))
(if (functionp 'which-key-mode) (which-key-mode))

;; 2018-10-29: Sets full file path in title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(defun jc/kill-new-file-name ()
  "Append the path of an open file into the kill ring"
  (interactive)
  (let ((bfn (buffer-file-name)))
    (when bfn
      (kill-new bfn)
      (message bfn))))
(define-key jc/right-map (kbd "M-n f") 'jc/kill-new-file-name) ;; put filename into [k]ill ring

;;; -- start in *scratch* buffer
(setq inhibit-startup-screen t)

;;; === START: spellcheck ===
(require 'flyspell-correct-helm)
(when (executable-find "aspell")
  (setq ispell-program-name "aspell")
  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")))
(defun jc/correct-at-word ()
  "If flyspell-mode, prefer it, otherwise use ispell-word"
  (interactive)
  (if flyspell-mode
      (flyspell-correct-at-point)
    (ispell-word)))
(define-key jc/c-1-map (kbd "C-2") 'flyspell-mode)
(define-key jc/c-1-map (kbd "C-4") 'jc/correct-at-word)
;;; === END: spellcheck ===

;;; === START: recentf ===
;; https://www.emacswiki.org/emacs/RecentFiles#toc9 - prevents blocking of emacs startup
(require 'recentf)
(setq recentf-auto-cleanup 'never)
(recentf-mode)
;;; === END: recentf ===

;;; === START: projectile ===
(define-key global-map (kbd "C-S-f") 'helm-projectile-rg)
(define-key jc/right-map (kbd "C-x C-f") 'helm-projectile-find-file)
;;; === END: projectile ===

(defun jc/git/log ()
  "Runs git status. Mostly a toy function to show how async-shell-command works."
  (interactive)
  (async-shell-command "PAGER=cat git log --pretty=oneline -l 25")
  )
(defun jc/scroll-up-command()
  "scroll-up-command followed by recentering"
  (interactive)
  (scroll-up-command)
  (recenter))
(defun jc/scroll-down-command()
  "move the page down by 1 line"
  (interactive)
  (scroll-down-command)
  (recenter))
;(global-set-key (kbd "C-v") 'jc/scroll-up-command)    ;; removed because they break copy-pasting for emacs running in a terminal
;(global-set-key (kbd "M-v") 'jc/scroll-down-command)  ;; removed because they break copy-pasting for emacs running in a terminal
  
(defun jc/hotkey1 ()
  "Hotkey 1"
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (bookmark-jump "work/todo")
  (bookmark-jump-other-window "people.org"))
(define-key jc/right-map (kbd "M-1") `jc/hotkey1)

