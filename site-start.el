; site-start.el --- personalization -*- lexical-binding: t -*-
;;; Installation --- 
;;;   echo '(load-file (expand-file-name "~/privprjs/dotfiles/site-start.el"))' >> ~/.emacs.d/init.el

;; Make emacs... start faster?  https://tychoish.com/post/towards-faster-emacs-start-times/
(defun jc/fast-startup ()
  (setq inhibit-startup-echo-area-message "tychoish")
  (setq inhibit-startup-message 't)
  (setq initial-major-mode 'fundamental-mode)
  (setq initial-scratch-message 'nil)
  (setq jit-lock-stealth-time nil)
  (setq jit-lock-defer-time nil)
  (setq jit-lock-defer-time 0.05)
  (setq jit-lock-stealth-load 200))
(jc/fast-startup)

;; Required packages
(defvar jc/package-refreshed-p nil "Sets to t once we ran package-refresh-contents once")
(defun jc/package-refresh-contents-once ()
  "Runs package-refresh-contents once per session"
  (unless jc/package-refreshed-p
    (package-refresh-contents)
    (setq jc/package-refreshed-p t)))
(defun jc/ensure-packages (&rest packages)
  "Installs the listed packages."
  (dolist (package packages)
    (unless (package-installed-p package)
      (jc/package-refresh-contents-once)
      (package-install package))))
(package-initialize)
(setq package-archives '(("melpa" . "http://melpa.org/packages/")))
(defun jc/init/installs ()
  "Installs favorite packages"
  (jc/ensure-packages
   'flycheck
   'flyspell-correct-helm 'helm 'helm-rg ;; helm is a super nice completion system
   'magit
   'php-mode                    ;; 2019-09-11 php? I'm doing this because of work :(
   'projectile 'helm-projectile ;; 2019-05-22 tried and loved it
   'python-mode
   'python-black                ;; 2020-10-31 tried and liked it
   'go-playground		;; 2019-05-27 tried and loved it
   'graphviz-dot-mode
   'markdown-mode
   ;;'origami                   ;; 2020-11-02 turn off -- do I use it?
   'terraform-mode              ;; 2019-10-10 trying terraform
   'try				;; 2019-07-15 allows one to try packages without installing them
   'which-key			;; 2019-07-15 tried and liked it   
   ))
(jc/init/installs)

;; library functions
(defun jc/file-readlines (file)
  "Returns contents of file as list of strings"
  (if (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (split-string (buffer-string) "\n" t))
    ))

(defvar jc/scratch-buffer-name "qweqwe" "Name of a scratch buffer that is transient")
(defun jc/open-scratch ()
  "Opens the buffer named by jc/scratch-buffer-name"
  (interactive)
  (switch-to-buffer jc/scratch-buffer-name))

;; fix paths
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin")) ;; Needed for M-x shell-command
(setenv "PATH" (concat (getenv "PATH") (concat ":" (expand-file-name "~/go/bin"))))
(setenv "PATH" (concat (getenv "PATH") ":/Library/Frameworks/Python.framework/Versions/3.6/bin/mypy")) ;; hack for mac os
(add-to-list 'exec-path "/usr/local/bin")                  ;; Needed for (executable-find ...)
(add-to-list 'exec-path (expand-file-name "~/go/bin"))
(add-to-list 'exec-path "/Library/Frameworks/Python.framework/Versions/3.6/bin/mypy") ;; hack for mac os

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
(define-key jc/left-map (kbd "M-q") 'jc/open-scratch)

(defun jc/init/misc-macOS ()
  "macOS misc customizations"
  (custom-set-variables
   '(flycheck-python-pycompile-executable (expand-file-name "~/.pyenv/shims/python"))
   '(flycheck-python-pylint-executable (expand-file-name "~/.pyenv/shims/pylint"))
   '(flycheck-python-flake8-executable (expand-file-name "~/.pyenv/shims/flake8")))
  (setq mac-command-modifier 'meta) ;; so the Alt key on WASD Code can be used for 'M-x'
  (setq mac-option-modifier 'super) ;; so the key left of Alt on WAS Code can be used for 'S-p'
  (global-unset-key (kbd "C-x m"))  ;; I'll never compose-mail on emacs
  (global-unset-key (kbd "s-w"))    ;; macOS: frequenly leads to accidental killing frames
  (global-unset-key (kbd "s-n"))   ;; macOS: frequenly leads to accidental new frames
  )

(defun jc/init/misc-linux()
  "linux misc customizations"
  (custom-set-variables
   '(flycheck-python-flake8-executable "/home/jcheng/.venv/privmono/bin/flake8")
   '(flycheck-python-mypy-executable "/home/jcheng/.venv/privmono/bin/mypy")
   '(flycheck-python-mypy-ini "/home/jcheng/privprjs/privmono/mypy.ini")
   '(flycheck-proselint-executable "/home/jcheng/.venv/privmono/bin/proselint")
   ))

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
   (jc/init/misc-macOS)
   (setq default-frame-alist '((top . 0) (left . 0) (height . 60) (width . 160))))
  ((gnu/linux)
   (message "gnu/linux")
   (jc/init/misc-linux)
   (setq default-frame-alist '((top . 0) (left . 0) (height . 61) (width . 217)))
   (set-face-attribute 'default (selected-frame))))

;; === START: org-mode ===
(defun jc/org-mode-hook ()
  "org-mode hooks. auto-fill has been useful."
  (define-key org-mode-map (kbd "C-c C-0") 'org-mark-ring-goto)
  (define-key org-mode-map (kbd "C-c C-n") nil) ;; next heading, but cut across different levels
  (define-key org-mode-map (kbd "C-c C-p") nil) ;; prev heading
  (define-key org-mode-map (kbd "C-c s") 'org-show-subtree)
  (define-key org-agenda-mode-map (kbd "M-s M-s") 'org-save-all-org-buffers)
  (define-key org-mode-map (kbd "C-c C-x C-f") nil) ;; Unset org-emphasize, conflicts with search
  (set-fill-column 120))
(add-hook 'org-mode-hook 'jc/org-mode-hook)

(defun jc/org-agenda-list-post (x)
  "post advice for org-agenda-list"
  (delete-other-windows))

(defun jc/init/org-mode ()
  (define-key jc/left-map (kbd "M-f") 'org-last-heading-same-level-jc)
  (define-key jc/left-map (kbd "M-b") 'org-switchb)
  (define-key jc/left-map (kbd "M-a") 'org-agenda-list)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c C-x C-f") 'helm-projectile-find-file)
  (advice-add 'org-agenda-list :filter-return #'jc/org-agenda-list-post)
  (setq org-archive-location "~/org/archive/%s_archive.org::datetree/* Finished Tasks"
	org-startup-folded 'content ;; https://orgmode.org/manual/Initial-visibility.html#Initial-visibility
	org-startup-indented t      ;; https://orgmode.org/manual/Clean-view.html
	org-src-fontify-natively t
	org-src-tab-acts-natively t
	) ; unclutter directories with org files
  )
(jc/init/org-mode)

(defun jc/org-append-agenda-file (newfile)
  "Append a file, if non-present, to ~/.org-jc.txt; 2019-11-25 - I am only keeping this function as a reference on elisp code. I hardly ever use it."
  (interactive)
  (let ((org-agendas-file "~/.org-jc.txt"))
    (if (file-exists-p org-agendas-file)
	(let ((flist (jc/file-readlines org-agendas-file)))          ; read org-jc.txt
	  (unless (seq-contains flist newfile)
	    (write-region newfile nil org-agendas-file 'append))
	  ))))
(defun jc/org-append-agenda-file-this ()
  "Append this buffer to ~/.org-jc.txt; 2019-11-25 - I am only keeping this function as a reference on elisp code. I hardly ever use it."
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

(defun jc/org-setup()
  "org-mode customizations"
  (setq org-agenda-files "~/.org-jc.txt")
  (custom-set-variables
   ;; (org-todo-keywords '((sequence "REPORT" "BUG" "KNOWNCAUSE" "|" "FIXED"))) ;; 2020-01-02 - example
   '(org-todo-keywords '((sequence "TODO" "|" "DONE")
			 (sequence "WK_CODING" "WK_PENDING_PR" "|" "WK_MERGED")
			 (sequence "|" "CANCELED")))
					; themes
   '(ansi-color-names-vector
     ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
   '(custom-enabled-themes (quote (manoj-dark)))

   ;; 2020-08-14 Graphviz
   '(graphviz-dot-auto-preview-on-save t)
   '(graphviz-dot-preview-extension "svg")
   '(org-fontify-quote-and-verse-blocks t)
   )
  (require 'org-tempo)  ;; 2020-08-20 < s TAB to create #+begin_src blocks
  (setq org-use-speed-commands t) ;; 2019-02-26 Trying this (use 'n' and 'p' to navigate up and down)
  )
(jc/org-setup)
;; === STOP: org-mode ===

;; === START: origami-mode ===
(defun jc/init/origami-mode ()
  "Init-time customizations for origami-mode"
  (add-hook 'emacs-lisp-mode-hook 'origami-mode)
  (add-hook 'python-mode-hook 'origami-mode)
  (setq origami-show-fold-header t))
(jc/init/origami-mode)
;; === STOP: origami-mode ===

;; === START: go-mode ===
(defun jc/init/go-mode ()
  "Init-time customizations for go-mode"
  (setq go-test-args "-v")
  (setq godoc-at-point-function 'godoc-gogetdoc)
  (unless (executable-find "gogetdoc")
    (message "gogetdoc not installed! (go get -u -v github.com/zmb3/gogetdoc)"))
  (defun jc/go-mode-hook ()
    (define-key jc/left-map (kbd "M-d") 'godoc-at-point)
    (define-key jc/left-map (kbd "C-d") 'godoc))
  (add-hook 'go-mode-hook 'jc/go-mode-hook)
  (add-hook 'before-save-hook 'gofmt-before-save)
  )
(jc/init/go-mode)
;; === STOP: go-mode ===

;; === START: terraform-mode ===
(defun jc/terraform-mode-init()
  "Init-time customizations for terraform-mode"
  (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode)
  )
(jc/terraform-mode-init)
;; === STOP: terraform-mode ===

;; === START: python-mode ===
(defun jc/python-init()
  "Init-time customizations for Python"
  (custom-set-variables
   '(python-shell-interpreter "python3"))
  (if (functionp 'python-black-on-save-mode) (add-hook 'python-mode-hook 'python-black-on-save-mode))
  )
(jc/python-init)

;; === STOP: python-mode ===

;;; enable emacsclient support unless we're running 'emacs-nox'
(unless
    (string-match "emacs-nox" (cl-first command-line-args))
  ;; runs emacs server
  (server-start)
  ;; Ubuntu: run a no-op command to bring the window into focus
  (add-hook 'server-visit-hook (lambda() (message " "))))

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
(defun jc/init/misc ()
  (setq column-number-mode t)
  (setq imenu-auto-rescan t)
  (setq confirm-kill-emacs 'y-or-n-p)
  (setq register-preview-delay 0)
  (setq scroll-error-top-bottom t)
  (setq show-paren-delay 0)
  (custom-set-variables
   '(fill-column 120)
   '(auto-revert-interval 1)
   '(kill-whole-line t)
   '(ibuffer-never-show-predicates '("^\\*dire" "^\\*") nil (ibuf-ext))
   '(mode-require-final-newline nil)
   '(sgml-basic-offset 4)
   '(visible-bell))
  (show-paren-mode 1)
  (global-auto-revert-mode t)
  (global-eldoc-mode -1)
  ; (auto-save-visited-mode)  ;; disabled on 2021-01-31 to favor save-some-buffers instead
  (global-set-key (kbd "M-s M-s") 'save-buffer) ;; left hand saver; my left pinky is killing me from hitting ctrl all the time.
  (global-set-key (kbd "M-S") 'save-buffer) ;; left hand saver; my left pinky is killing me from hitting ctrl all the time.
  (define-key jc/right-map (kbd "M-j") 'jsnice-jc) ;; [j]son indent
  (define-key jc/right-map (kbd "M-p") 'bookmark-jump)
  (define-key jc/right-map (kbd "M-h") 'command-history)
  (define-key jc/right-map (kbd "M-k") 'kill-current-buffer)
  (define-key jc/left-map (kbd "M-r") 'revert-buffer)
  (define-key jc/left-map (kbd "M-s") 'whitespace-mode) ;; toggle [s]paces
  (define-key jc/right-map (kbd "M-i") 'helm-semantic-or-imenu)
  (define-key jc/left-map (kbd "t") 'origami-toggle-all-nodes)  ;; [t]oggle
  (define-key jc/c-1-map (kbd "C-r") 'point-to-register) ;; [r]emember
  (define-key jc/c-1-map (kbd "C-g") 'jump-to-register)  ;; [g]oto
  (define-key jc/right-map (kbd "M-b") 'ibuffer)  ;; i[b]uffer
  )
(jc/init/misc)
;; === START: aliases ===
(defun jc/init/aliases ()
  (defalias 'yes-or-no-p 'y-or-n-p)
  (defalias 'list-buffers 'ibuffer)
  (defalias 'rg 'helm-projectile-rg)
  (defalias 'cst 'org-cut-subtree))
(jc/init/aliases)
;; === END: aliases ===


;; 2020-10-04 Trying out from from https://gist.github.com/mookid/64941602b840ca5ce81e2b3017d1e0a5
;; 2020-10-05 Stopped using repeatable zap-to-char since it breaks zapping backward
(define-key global-map [remap zap-to-char] 'zap-up-to-char)

;; === START: markdown-mode ===
(defun jc/markdown-mode-hook()
  (define-key markdown-mode-map (kbd "M-n") nil)
  (set-fill-column 120))
(add-hook 'markdown-mode-hook 'jc/markdown-mode-hook)
;; === END: markdown-mode ===
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

;; (if (functionp 'global-company-mode) (global-company-mode)) 2019-11-20 Disabled for testing
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
(define-key jc/right-map (kbd "M-f") 'jc/kill-new-file-name) ;; put filename into [k]ill ring

;;; -- start in *scratch* buffer
(setq inhibit-startup-screen t)

;;; === START: helm ===
(defun jc/init/helm ()
  (define-key jc/right-map (kbd "f") 'helm-rg)
  (let ((modes '(helm-mode ivy-mode)))
    (funcall (cl-first (seq-filter 'functionp modes))))
  (when (functionp 'helm-mode)
    (helm-mode)
    (define-key global-map (kbd "C-x C-f") 'helm-find-files)
    (define-key global-map (kbd "C-x b") 'helm-mini)
    (define-key global-map (kbd "M-x") 'helm-M-x))
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; 2020-02-21 try http://tuhdo.github.io/helm-intro.html 
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
  (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
  ;; (helm-autoresize-mode t) ;; comment out, the selected entry moving down as the buffer resizes is too jarring
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
  (custom-set-variables
   ;; 2020-11-26 Use helm in separate frame 2021-01-11 somewhat didn't enjoy it
   ;;   '(helm-display-function 'helm-display-buffer-in-own-frame)
   '(helm-use-undecorated-frame-option nil)
   '(helm-display-buffer-height 40)
   '(helm-display-buffer-width 200)
   ;; END:

   '(helm-buffer-max-length 40)
   
   '(helm-boring-buffer-regexp-list
     '("\\` " "\\`\\*helm" "\\`\\*Echo Area" "\\`\\*Minibuf" "\\`\\*"))
   '(helm-ff-skip-boring-files t)))
(jc/init/helm)
;;; === END: helm ===

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
(define-key jc/c-1-map (kbd "C-1") 'flyspell-buffer)
(define-key jc/c-1-map (kbd "C-2") 'flyspell-mode)
(define-key jc/c-1-map (kbd "C-4") 'jc/correct-at-word)
;;; === END: spellcheck ===

;;; === START: recentf ===
;; https://www.emacswiki.org/emacs/RecentFiles#toc9 - prevents blocking of emacs startup
(defun jc/init/recentf ()
  "recentf init"
  (require 'recentf)
  (custom-set-variables
   '(recentf-auto-cleanup 'never)
   '(recentf-max-saved-items 1000))
  (recentf-mode))
(jc/init/recentf)
;;; === END: recentf ===

;;; === START: projectile ===
(define-key global-map (kbd "C-S-f") 'helm-projectile-rg)
(define-key jc/right-map (kbd "C-x C-f") 'helm-projectile-find-file)
(helm-projectile-on)
;;; === END: projectile ===

;;; === START: flycheck ===
(custom-set-variables
 '(flycheck-json-python-json-executable "python3")
 '(flycheck-python-pycompile-executable "python3") 
 '(flycheck-disabled-checkers '(python-pylint go-golint)) ;; 2020-02-26 trying flycheck for go
 )
(add-hook 'python-mode-hook 'flycheck-mode)
(add-hook 'go-mode-hook 'flycheck-mode)

;; 2020-02-26 Tried golint and got numerous false positives, e.g., "document your functions". Not used.
(defun jc/go/flycheck-init ()
  "Activates golint for flycheck"
  (add-to-list 'load-path (concat (getenv "HOME")  "/go/src/golang.org/x/lint/misc/emacs/"))
  (require 'golint)
  (add-hook 'go-mode-hook `golint))

;;; === END: flycheck === 

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
  "An example of binding some function to a hot key"
  (interactive)
  (delete-other-windows)
  (split-window-horizontally))
(define-key jc/right-map (kbd "M-1") `jc/hotkey1)

(defun jc/window-width-66 ()
  (interactive)
  (let ((target (- (floor (* (frame-width) 0.66)) (window-width))))
    (window-resize nil target t)))
(defun jc/enlarge-window-horizontally ()
  (interactive)
  (enlarge-window-horizontally 15))
(defun jc/shrink-window-horizontally ()
  (interactive)
  (shrink-window-horizontally 15))
(define-key jc/right-map (kbd "M-w") `jc/window-width-66)
(define-key jc/right-map (kbd "M-.") `jc/enlarge-window-horizontally)
(define-key jc/right-map (kbd "M-,") `jc/shrink-window-horizontally)

(defun jc/org-time-stamp-inactive ()
  "Run org-time-stamp-inactive in all modes"
  (interactive)
  (org-timestamp-inactive))
(define-key jc/right-map (kbd "!") `jc/org-time-stamp-inactive)

(defun jc/emacs-notes ()
  "Open up emacs notes in the other window"
  (interactive)
  (bookmark-jump "emacs_notes.org" 'switch-to-buffer-other-window)
  (outline-show-all))
(define-key jc/right-map (kbd "M-e") `jc/emacs-notes)
