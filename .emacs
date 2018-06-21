;;;
;;; Mike's .emacs file
;;;

;; Load common lisp extensions
(eval-when-compile (require 'cl-lib))

;; Fuck you unless. Just fuck you.
(defalias 'ifnot 'unless)

;;------------------------------------------------------------------------------
;; Packages

(setq package-archives '(("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
(setq package-archive-priorities '(("melpa-stable" . 3)
                                   ("melpa" . 2)
                                   ("gnu" . 1)))
(setq package-menu-hide-low-priority t)

(ifnot (file-directory-p (concat user-emacs-directory "elpa"))
  (package-refresh-contents))

(package-initialize)
(ifnot (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-always-ensure t)

;; From package.el#package--get-deps:1703
(defun miken-package-get-deps (pkg &optional only)
  "Get all packages on which PKG depends"
  (interactive)
  (let* ((pkg-desc (cadr (assq pkg package-alist)))
         (direct-deps (cl-loop for p in (package-desc-reqs pkg-desc)
                               for name = (car p)
                               when (assq name package-alist)
                               collect name))
         (indirect-deps (ifnot (eq only 'direct)
                          (cl-remove-duplicates
                           (cl-loop for p in direct-deps
                                    append (miken-package-get-deps p))))))
    (cl-case only
      (direct   direct-deps)
      (separate (list direct-deps indirect-deps))
      (indirect indirect-deps)
      (t        (cl-remove-duplicates (append direct-deps indirect-deps))))))

(defun miken-package-get-dependees (dependency)
  "Get all packages which depend on DEPENDENCY"
  (interactive)
  (cl-loop for pkg in (cl-remove-duplicates package-activated-list)
           for deps = (miken-package-get-deps pkg)
           when (memq dependency deps)
           collect pkg))
;; (miken-package-get-dependees 'dash)
;; TODO: Debug this error
;; Debugger entered--Lisp error: (wrong-type-argument arrayp nil)
;;   aref(nil 0)
;;   (memq (aref pkg-desc 0) cl-struct-package-desc-tags)
;;   (and (memq (aref pkg-desc 0) cl-struct-package-desc-tags))
;;   (or (and (memq (aref pkg-desc 0) cl-struct-package-desc-tags)) (signal (quote wrong-type-argument) (list (quote package-desc) pkg-desc)))
;;   (progn nil (or (and (memq (aref pkg-desc 0) cl-struct-package-desc-tags)) (signal (quote wrong-type-argument) (list (quote package-desc) pkg-desc))) (aref pkg-desc 4))
;;   (let* ((--cl-var-- (progn nil (or (and (memq (aref pkg-desc 0) cl-struct-package-desc-tags)) (signal (quote wrong-type-argument) (list (quote package-desc) pkg-desc))) (aref pkg-desc 4))) (p nil) (name nil) (--cl-var-- nil)) (while (consp --cl-var--) (setq p (car --cl-var--)) (setq name (car p)) (if (assq name package-alist) (progn (setq --cl-var-- (cons name --cl-var--)))) (setq --cl-var-- (cdr --cl-var--))) (nreverse --cl-var--))
;;   (let* ((pkg-desc (car (cdr (assq pkg package-alist)))) (direct-deps (let* ((--cl-var-- (progn nil (or ... ...) (aref pkg-desc 4))) (p nil) (name nil) (--cl-var-- nil)) (while (consp --cl-var--) (setq p (car --cl-var--)) (setq name (car p)) (if (assq name package-alist) (progn (setq --cl-var-- ...))) (setq --cl-var-- (cdr --cl-var--))) (nreverse --cl-var--))) (indirect-deps (if (eq only (quote direct)) nil (cl-remove-duplicates (let* ((--cl-var-- direct-deps) (p nil) (--cl-var-- nil)) (while (consp --cl-var--) (setq p ...) (setq --cl-var-- ...) (setq --cl-var-- ...)) (nreverse --cl-var--)))))) (cond ((eql only (quote direct)) direct-deps) ((eql only (quote separate)) (list direct-deps indirect-deps)) ((eql only (quote indirect)) indirect-deps) (t (cl-remove-duplicates (append direct-deps indirect-deps)))))
;;   miken-package-get-deps(with-editor)
;;   (setq deps (miken-package-get-deps pkg))
;;   (while (consp --cl-var--) (setq pkg (car --cl-var--)) (setq deps (miken-package-get-deps pkg)) (if (memq dependency deps) (progn (setq --cl-var-- (cons pkg --cl-var--)))) (setq --cl-var-- (cdr --cl-var--)))
;;   (let* ((--cl-var-- (cl-remove-duplicates package-activated-list)) (pkg nil) (deps nil) (--cl-var-- nil)) (while (consp --cl-var--) (setq pkg (car --cl-var--)) (setq deps (miken-package-get-deps pkg)) (if (memq dependency deps) (progn (setq --cl-var-- (cons pkg --cl-var--)))) (setq --cl-var-- (cdr --cl-var--))) (nreverse --cl-var--))
;;   miken-package-get-dependees(dash)
;;   eval-region(2243 2278 t #[257 "\300\242b\210\301\207" [(2278) (miken-package-get-dependees (quote dash))] 2 "\n\n(fn IGNORE)"])  ; Reading at buffer position 2277
;;   elisp--eval-defun()
;;   eval-defun(nil)
;;   funcall-interactively(eval-defun nil)
;;   #<subr call-interactively>(eval-defun nil nil)
;;   ad-Advice-call-interactively(#<subr call-interactively> eval-defun nil nil)
;;   apply(ad-Advice-call-interactively #<subr call-interactively> (eval-defun nil nil))
;;   call-interactively(eval-defun nil nil)
;;   command-execute(eval-defun)

;;------------------------------------------------------------------------------
;; OS settings

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta
        mac-option-modifier 'super ))

(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell
    :init (exec-path-from-shell-initialize)))

(add-to-list 'exec-path "/usr/local/bin")

;;------------------------------------------------------------------------------
;; Included lisp and required libraries

;; cl-labels is like let for functions
(cl-labels
    ((add-path (p) (add-to-list 'load-path (concat user-emacs-directory p))))
  (add-path "lisp")
  (add-path "themes") )

(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes"))

;;------------------------------------------------------------------------------
;; Global config

(setq inhibit-startup-message t)

;; We don't need no stinkin' GUI
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(setq make-backup-files nil)
(setq ring-bell-function 'ignore)

;; Disable some annoying prompts
(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil)
(setq ido-create-new-buffer 'always)

(setq-default cursor-type 'bar)
(setq-default blink-cursor-blinks 0)
(setq blink-cursor-interval 0.8)

;; Scrolling
(setq scroll-preserve-screen-position t)
(setq mouse-wheel-progressive-speed nil)

;; Replaces M-x to run commands
(use-package smex
  :config (smex-initialize)
  :bind (("C-x m" . smex)
         ("C-x C-m" . smex)))

;;------------------------------------------------------------------------------
;; Global modes

(setq line-number-mode t)
(setq column-number-mode t)
(show-paren-mode 1)
(global-hl-line-mode 1) ;; Highlight current line
(setq comment-style 'indent)
(electric-pair-mode)

(global-set-key (kbd "C-<f5>") 'linum-mode)

;; DA-DA-DA DAAA, daa daa DAAT duh-DAAAAAA!
(winner-mode)

(use-package discover :config (global-discover-mode 1))
(use-package drag-stuff
  :config (drag-stuff-global-mode)
  :bind (("M-<up>" . drag-stuff-up)
         ("M-<down>" . drag-stuff-down)))
(use-package rbenv :config (global-rbenv-mode))

;;------------------------------------------------------------------------------
;; Saving

(global-set-key [f9] 'save-buffer)
(desktop-save-mode t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(global-auto-revert-mode 1)

;;------------------------------------------------------------------------------
;; Font size / text size

(defun miken-font-size ()
  (interactive)
  (let ((font-size (cond
                    ((<= (display-pixel-height) 800) "14")
                    ((<= (display-pixel-height) 1200) "16")
                    ((<= (display-pixel-height) 1440) "18")
                    ((<= (display-pixel-height) 1920) "18")
                    (t "20") )))
    (set-face-attribute 'default nil :font (concat "Inconsolata-" font-size)) ))
(miken-font-size)

;;------------------------------------------------------------------------------
;; Color themes

(setq rainbow-x-colors nil)

(use-package rainbow-mode
  :defer t
  :config
  (add-hook 'emacs-lisp-mode-hook
            (lambda () (if (string-match ".*theme.*" (buffer-name))
                           (rainbow-mode) ))))

(defun miken-override-theme (theme)
  "Clear out the active themes and load a theme freshly"
  (interactive "sOverride with custom theme: ")
  (while custom-enabled-themes
    (disable-theme (car custom-enabled-themes)))
  (load-theme (intern theme) t nil))

(use-package railscasts-theme)

;;------------------------------------------------------------------------------
;; Sound

(ifnot (and (fboundp 'play-sound-internal)
            (subrp (symbol-function 'play-sound-internal)))
  (require 'play-sound))

(defun miken-lightsaber (opt)
  (interactive)
  (let ((action (if opt "up" "down")))
    (play-sound-file (concat user-emacs-directory "sounds/lightsaber-" action ".mp3"))))

;;------------------------------------------------------------------------------
;; Frame management

(setq default-frame-alist
      (append default-frame-alist
              '((cursor-color . "#FFFFFF")) ))

(global-set-key (kbd "M-`") 'other-frame)

;; Full path in frame title
(if window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))

(defun miken-make-frame-command ()
  "Play a lightsaber ignition sound when making a new frame"
  (interactive)
  (miken-lightsaber t)
  (make-frame-command))
(global-set-key (kbd "C-x 5 2") 'miken-make-frame-command)

(defun miken-delete-frame ()
  "Play a lightsaber deactivate sound when killing a frame"
  (interactive)
  (miken-lightsaber nil)
  (delete-frame))
(global-set-key (kbd "C-x 5 0") 'miken-delete-frame)

(defun miken-maximus-frame ()
  "Stretch a frame across two monitors"
  (interactive)
  (miken-font-size)
  (set-frame-position (selected-frame) 5 25)
  (set-frame-size (selected-frame) 237 93))

(global-set-key (kbd "C-M-s-f") 'miken-maximus-frame)

;;------------------------------------------------------------------------------
;; Window management

(use-package workgroups
  :config
  (workgroups-mode 1)
  (setq wg-morph-on nil)
  (let ((workgroups-file (concat user-emacs-directory "workgroups")))
    (if (file-exists-p workgroups-file) (wg-load workgroups-file))))

(use-package zygospore
  :bind ("C-x 1" . zygospore-toggle-delete-other-windows))

(global-set-key (kbd "C-x =") 'balance-windows)
;; Previously bound to C-x =
(global-set-key (kbd "C-x +") 'what-cursor-position)

;; Move cursor to other window
(global-set-key (kbd "M-s-h") 'windmove-left)
(global-set-key (kbd "M-s-j") 'windmove-down)
(global-set-key (kbd "M-s-k") 'windmove-up)
(global-set-key (kbd "M-s-l") 'windmove-right)

;; Move buffer to other window
(use-package buffer-move
  :bind
  (("C-s-h" . buf-move-left)
   ("C-s-j" . buf-move-down)
   ("C-s-k" . buf-move-up)
   ("C-s-l" . buf-move-right)))

;; Window resizing
(global-set-key (kbd "M-s-<up>") (lambda () (interactive) (enlarge-window 2)))
(global-set-key (kbd "M-s-<down>") (lambda () (interactive) (enlarge-window -2)))
(global-set-key (kbd "M-s-<left>") (lambda () (interactive) (enlarge-window -2 t)))
(global-set-key (kbd "M-s-<right>") (lambda () (interactive) (enlarge-window 2 t)))

;;------------------------------------------------------------------------------
;; Buffer management

(global-set-key (kbd "C-c k") 'kill-buffer-and-window)
(global-set-key (kbd "C-c C-k") 'kill-buffer-and-window)

(global-set-key (kbd "C-c b") 'switch-to-buffer-other-window)
(global-set-key (kbd "C-c C-b") 'switch-to-buffer-other-window)

;; Already bound to C-x b
(global-set-key (kbd "C-x C-b") 'switch-to-buffer)

;; Previously bound to C-x C-b
(global-set-key (kbd "C-x C-l") 'list-buffers)

(setq-default uniquify-buffer-name-style 'post-forward)

;;------------------------------------------------------------------------------
;; Region management

(use-package expand-region
  :bind
  (("s-<up>" . er/expand-region)
   ("s-<down>" . er/contract-region)))

;; Delete region when you start typing
(pending-delete-mode t)

;;------------------------------------------------------------------------------
;; Paragraph management

(setq-default fill-column 100)

(defun endless/forward-paragraph (&optional n)
  "Advance just past next blank line."
  (interactive "p")
  (let ((m (use-region-p))
        (para-commands
         '(endless/forward-paragraph endless/backward-paragraph)))
    ;; Only push mark if it's not active and we're not repeating.
    (or m
        (not (member this-command para-commands))
        (member last-command para-commands)
        (push-mark))
    ;; The actual movement.
    (dotimes (_ (abs n))
      (if (> n 0)
          (skip-chars-forward "\n[:blank:]")
        (skip-chars-backward "\n[:blank:]"))
      (if (search-forward-regexp
           "\n[[:blank:]]*\n[[:blank:]]*" nil t (cl-signum n))
          (goto-char (match-end 0))
        (goto-char (if (> n 0) (point-max) (point-min)))))
    ;; If mark wasn't active, I like to indent the line too.
    (ifnot m
      (indent-according-to-mode)
      ;; This looks redundant, but it's surprisingly necessary.
      (back-to-indentation))))

(defun endless/backward-paragraph (&optional n)
  "Go back up to previous blank line."
  (interactive "p")
  (endless/forward-paragraph (- n)))

(global-set-key (kbd "M-a") 'endless/backward-paragraph)
(global-set-key (kbd "M-e") 'endless/forward-paragraph)

;;------------------------------------------------------------------------------
;; Projectile

(use-package ag
  :config
  (setq ag-reuse-window t))

(use-package projectile
  :config
  (projectile-global-mode)
  (setq projectile-mode-line " Pj")
  :bind
  (("M-s-f" . projectile-find-file)
   ("M-s-v" . projectile-vc)
   ("M-s-b" . projectile-ibuffer)
   ("M-s-s" . projectile-ag)))

(recentf-mode)

;;------------------------------------------------------------------------------
;; Autocomplete/auto-complete

(use-package auto-complete
  :bind
  (("M-/" . auto-complete)
   ("C-M-/" . ac-fuzzy-complete))
  :config
  (require 'auto-complete-config)
  (add-to-list 'ac-dictionary-directories (concat user-emacs-directory "auto-complete/ac-dict"))
  (ac-config-default)
  (setq-default completion-ignore-case 1)
  (add-to-list 'ac-modes 'elixir-mode 'rjsx-mode))

;;------------------------------------------------------------------------------
;; ido and flx

(ido-mode 1)
(ido-everywhere 1)
(setq ido-enable-flex-matching t)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)

;; From: http://endlessparentheses.com/Ido-Bury-Buffer.html
(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-completion-map
              (kbd "C-b") 'endless/ido-bury-buffer-at-head)))

(defun endless/ido-bury-buffer-at-head ()
  "Bury the buffer at the head of `ido-matches'."
  (interactive)
  (let ((enable-recursive-minibuffers t)
        (buf (ido-name (car ido-matches)))
        (nextbuf (cadr ido-matches)))
    (when (get-buffer buf)
      ;; If next match names a buffer use the buffer object;
      ;; buffer name may be changed by packages such as uniquify.
      (when (and nextbuf (get-buffer nextbuf))
        (setq nextbuf (get-buffer nextbuf)))
      (bury-buffer buf)
      (if (bufferp nextbuf)
          (setq nextbuf (buffer-name nextbuf)))
      (setq ido-default-item nextbuf
            ido-text-init ido-text
            ido-exit 'refresh)
      (exit-minibuffer))))

(use-package ido-completing-read+ :config (ido-ubiquitous-mode))
(use-package flx-ido :config (flx-ido-mode 1))

;;------------------------------------------------------------------------------
;; Searching / Replacing

(global-set-key (kbd "s-s") 'isearch-forward-regexp)
(global-set-key (kbd "s-r") 'isearch-backward-regexp)

(use-package visual-regexp :bind ("C-x M-r" . vr/query-replace))

;;------------------------------------------------------------------------------
;; Jumping / Tags

(use-package avy :bind ("M-s-g" . avy-goto-word-or-subword-1))

(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "M-,") 'pop-tag-mark)

;;------------------------------------------------------------------------------
;; Neotree

(use-package neotree
  :bind ([f8] . miken-neotree)
  :config
  (setq neo-smart-open t
        neo-autorefresh nil)
  (defun miken-neotree ()
    "Open neotree using the projectile-project-root."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (neotree-toggle)
      (when (and project-dir (neo-global--window-exists-p))
        (neotree-dir project-dir)
        (neotree-find file-name)))) )

;;------------------------------------------------------------------------------
;; Refactoring

(use-package ruby-refactor
  :defer t
  :config (setq ruby-refactor-add-parens t))

;;------------------------------------------------------------------------------
;; Language modes config

;; Always spaces, always 2, always line numbers
(setq-default indent-tabs-mode nil
              tab-width 2)
(add-hook 'prog-mode-hook (lambda () (linum-mode)))

;; shell
(add-hook 'sh-mode-hook
          (lambda () (setq-default sh-basic-offset 2
                                   sh-indentation 2) ))

;; elisp
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (define-key emacs-lisp-mode-map (kbd "RET") 'newline-and-indent) ))

;; JavaScript etc.
(use-package js2-mode
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
  (customize-set-variable 'js2-basic-offset 2)
  (add-hook 'js2-mode-hook
            (lambda () (define-key js2-mode-map (kbd "RET") 'newline-and-indent) )))

(add-hook 'js-mode-hook
          (lambda () (define-key js-mode-map (kbd "RET") 'newline-and-indent))
          (customize-set-variable 'js-indent-level 2))
;; JSX
(use-package rjsx-mode
  :config
  (customize-set-variable 'jsx-indent-level 2))

(defun miken-setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1))
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  ;; (company-mode +1))

;; TypeScript
(use-package tide
  :config
  (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook #'miken-setup-tide-mode)
  (setq typescript-indent-level 2))

;; TSX
(use-package web-mode
  :init
  (setq standard-indent 2)
  :config
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (miken-setup-tide-mode)))))

;; HTML
(global-set-key (kbd "C-c e") 'sgml-close-tag)

;; CSS
(add-hook 'css-mode-hook
          (lambda ()
            (rainbow-mode)
            (setq css-indent-offset 2)))

;; SASS
(use-package sass-mode
  :defer t
  :config
  (add-hook 'sass-mode-hook
            (lambda ()
              (rainbow-mode)
              (setq css-indent-offset 2))))

;; SCSS
(use-package scss-mode
  :defer t
  :config
  (setq scss-compile-at-save nil)
  (add-hook 'scss-mode-hook
            (lambda ()
              (rainbow-mode)
              (setq css-indent-offset 2))))

;; Ruby
(use-package ruby-hash-syntax :defer t)
(use-package ruby-end :config (setq ruby-end-insert-newline nil))

(use-package enh-ruby-mode
  :config
  (add-to-list 'auto-mode-alist
               '("\\.\\(?:gemspec\\|irbrc\\|gemrc\\|pryrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . enh-ruby-mode))
  (add-to-list 'auto-mode-alist
               '("\\(Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . enh-ruby-mode))
  (setq enh-ruby-hanging-brace-deep-indent-level 1)

  (defun miken-ruby-interpolate ()
    "In a double quoted string, interpolate."
    (interactive)
    (insert "#")
    (when (and (looking-back "\".*") (looking-at ".*\""))
      (insert "{}")
      (backward-char 1)))

  (add-hook 'enh-ruby-mode-hook
            (lambda ()
              (ruby-end-mode)
              (auto-complete-mode)
              (define-key enh-ruby-mode-map (kbd "RET") 'newline-and-indent)
              (define-key enh-ruby-mode-map (kbd "#") 'miken-ruby-interpolate) )))

;; C
(add-hook 'c-mode-hook (lambda () (setq tab-width 4)))

;; Java
(add-hook 'java-mode-hook (lambda () (setq tab-width 4)))

;; Markdown
(use-package markdown-mode
  :defer t
  :config
  (add-hook 'markdown-mode-hook
            (lambda () (miken-keys-minor-mode t)) ))

(use-package coffee-mode :defer t)
(use-package clojure-mode :defer t)
(use-package dockerfile-mode :defer t)
(use-package haml-mode :defer t)
(use-package haskell-mode :defer t)
(use-package scala-mode :defer t)
(use-package slim-mode :defer t)
(use-package yaml-mode :defer t)

;;------------------------------------------------------------------------------
;; Rails settings

(use-package projectile-rails
  :defer t
  :config
  (setq miken-rails-file-types
        '(;; Ruby
          ruby-mode-hook
          enh-ruby-mode-hook
          ;; JavaScript / CoffeeScript
          javascript-mode-hook js2-mode-hook coffee-mode-hook
          ;; Styles
          css-mode-hook sass-mode-hook scss-mode-hook
          ;; Markup
          html-mode-hook html-erb-mode-hook slim-mode-hook haml-mode-hook yaml-mode-hook))
  ;; Turn on projectile-rails-mode if we're in a rails project
  (dolist (hook miken-rails-file-types)
    (add-hook hook
              (lambda ()
                (if (and (projectile-project-p)
                         (file-exists-p (concat (projectile-project-root) "Gemfile")) )
                    (projectile-rails-mode) )))))

;; erb files
(use-package mmm-mode
  :config
  (setq mmm-global-mode 'maybe)
  (mmm-add-mode-ext-class 'html-erb-mode "\\.erb\\'" 'erb)
  (mmm-add-mode-ext-class 'html-erb-mode "\\.jst" 'ejs)
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . html-erb-mode))
  (add-to-list 'auto-mode-alist '("\\.jst'"  . html-erb-mode)))

;;------------------------------------------------------------------------------
;; rspec

(use-package rspec-mode
  :init
  (setq compilation-scroll-output nil)
  (setq rspec-use-rake-when-possible nil)
  :config
  (add-hook 'after-init-hook 'inf-ruby-switch-setup)
  (defun miken-rspec-toggle-flip ()
    (interactive)
    (split-window-below)
    (windmove-down)
    (rspec-toggle-spec-and-target))
  :bind
  ("M-s-t" . miken-rspec-toggle-flip))

;;------------------------------------------------------------------------------
;; Alchemist / elixir

(use-package alchemist
  :defer t
  :init
  (setq alchemist-key-command-prefix (kbd "C-c ,"))
  :config
  (defun miken-insert-iex-pry ()
    "Inserts the line `require IEx; IEx.pry'"
    (interactive)
    (miken-open-line-above)
    (insert "require IEx; IEx.pry"))
  (setq alchemist-mix-test-task "espec")
  (add-to-list 'elixir-mode-hook
               (defun auto-activate-ruby-end-mode-for-elixir-mode ()
                 (set (make-variable-buffer-local 'ruby-end-expand-keywords-before-re)
                      "\\(?:^\\|\\s-+\\)\\(?:do\\)")
                 (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers) nil)
                 (ruby-end-mode +1)))
  :bind (("M-s-p" . miken-insert-iex-pry)))

;;------------------------------------------------------------------------------
;; ediff setup

(add-hook 'ediff-quit-hook 'winner-undo)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;;------------------------------------------------------------------------------
;; Multi-term / shell config

;; For term-bind-key-alist
(use-package multi-term
  :config
  (defun miken-switch-to-or-create-shell-buffer (index)
    "Switches to *terminal<INDEX>* if it exists, or creates a new terminal."
    (interactive)
    (let ((term-name (concat "*terminal<" index ">*")))
      (if (get-buffer term-name)
          (switch-to-buffer term-name)
        (switch-to-buffer (multi-term)) )))

  (dolist (index '("1" "2" "3" "4" "5" "6" "7" "8" "9"))
    (global-set-key (kbd (concat "M-s-" index))
                    `(lambda () (interactive)
                       (miken-switch-to-or-create-shell-buffer ,index))))

  (dolist (key-command
           '(("M-<backspace>" . term-send-backward-kill-word)
             ("M-d" . term-send-forward-kill-word)))
    (add-to-list 'term-bind-key-alist key-command)))

;;------------------------------------------------------------------------------
;; Text manipulation

;; From the Yegge blog post "Saving Time"
(defun miken-fix-amazon-url ()
  "Minimizes the Amazon URL under the point.  You can paste an Amazon
  URL out of your browser, put the cursor in it somewhere, and invoke
  this method to convert it."
  (interactive)
  (and (search-backward "http://www.amazon.com" (point-at-bol) t)
       (search-forward-regexp
        ".+/\\([A-Z0-9]\\{10\\}\\)/[^[:space:]\"]+" (point-at-eol) t)
       (replace-match
        (concat "http://www.amazon.com/o/asin/"
                (match-string 1)
                (match-string 3) ))))

(defun miken-xml-format ()
  "Formats a region of XML to look nice"
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "xmllint --format -" (buffer-name) t) ))

(defun miken-json-format ()
  (interactive)
  (let ((begin (if mark-active (min (point) (mark)) (point-min)))
        (end (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region begin end "python -mjson.tool" (current-buffer) t)))

(defun miken-toggle-quotes ()
  "Toggle single quoted string to double or vice versa, and
  flip the internal quotes as well. Best to run on the first
  character of the string."
  (interactive)
  (save-excursion
    (move-beginning-of-line nil)
    (re-search-forward "[\"']")
    (backward-char 1)
    (let* ((start (point))
           (old-c (char-to-string (char-after start)))
           (new-c (if (string= old-c "'") "\"" "'")))
      (replace-match new-c)
      (search-forward old-c)
      (backward-char 1)
      (let ((end (point)))
        (replace-match new-c)
        (replace-string new-c old-c nil (1+ start) end)))))
(global-set-key (kbd "C-c t") 'miken-toggle-quotes)
(global-set-key (kbd "C-c C-t") 'miken-toggle-quotes)

(global-set-key (kbd "C-x \\") 'align-regexp)

;;------------------------------------------------------------------------------
;; Functions that should exist already

;; Never understood why Emacs doesn't have this function.
(defun miken-rename-buffer-and-file (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)) )
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn (rename-file name new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil) )))))

;; Never understood why Emacs doesn't have this function, either.
(defun miken-move-buffer-and-file (dir)
  "Moves both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
         (newname (concat dir "/" name)) )
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn (copy-file filename newname 1)
             (delete-file filename)
             (set-visited-file-name newname)
             (set-buffer-modified-p nil) t))))

;; Word count
(defun miken-count-words-buffer ()
  "Counts the number of words in the buffer."
  (interactive)
  (let ((count 0))
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (forward-word 1)
        (setq count (+ 1 count)) )
      (message "buffer contains %d words." count) )))

;;------------------------------------------------------------------------------
;; Custom functions

;; This is the greatest and best function ever.
(defun miken-reload ()
  "Reloads the .emacs file"
  (interactive)
  (load-file user-init-file))

(defun miken-copy-line-below (&optional n)
  "Duplicate current line, make more than 1 copy given a numeric argument"
  (interactive "p")
  (save-excursion
    (let ((current-line (thing-at-point 'line)))
      ;; When on any line except the last, insert a newline first
      (if (= 1 (forward-line 1))
          (insert "\n"))
      ;; now insert as many time as requested
      (while (> n 0)
        (insert current-line)
        (decf n) )))
  (forward-line))
(global-set-key (kbd "C-c d") 'miken-copy-line-below)
(global-set-key (kbd "C-c C-d") 'miken-copy-line-below)

;; Emulate vim's half-screen scrolling
(defun miken-window-half-height ()
  (max 1 (/ (+ 1 (window-height (selected-window))) 2)) )
(global-set-key (kbd "C-v")
                (lambda () (interactive)
                  (scroll-up (miken-window-half-height)) ))
(global-set-key (kbd "M-v")
                (lambda () (interactive)
                  (scroll-down (miken-window-half-height)) ))

(defun miken-open-line-above ()
  "Insert a newline above the current line and indent point."
  (interactive)
  (ifnot (bolp) (beginning-of-line))
  (newline)
  (forward-line -1)
  (indent-according-to-mode))
(global-set-key (kbd "C-c o") 'miken-open-line-above)
(global-set-key (kbd "C-c C-o") 'miken-open-line-above)

;; In the pipe, five-by-five
(defun miken-previous-line-five () (interactive) (forward-line -5))
(global-set-key (kbd "M-p") 'miken-previous-line-five)

(defun miken-next-line-five () (interactive) (forward-line 5))
(global-set-key (kbd "M-n") 'miken-next-line-five)

(defun miken-current-buffer-filepath ()
  "Put the current file path on the clipboard"
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)) )
      (message filename) )))
(global-set-key (kbd "C-c `") 'miken-current-buffer-filepath)

(defun miken-comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
   If no region is selected and current line is not blank, then comment current line.
   Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (not (region-active-p))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))
(global-set-key (kbd "M-;") 'miken-comment-dwim-line)

(defun miken-comment-dwim-line-and-move-down (&optional arg)
  "Comment the current line and move to the next line"
  (interactive)
  (miken-comment-dwim-line arg)
  (forward-line) )
(global-set-key (kbd "C-M-;") 'miken-comment-dwim-line-and-move-down)

(defun save-macro (name)
  "save a macro. Take a name as argument
     and save the last defined macro under
     this name at the end of your .emacs"
  (interactive "SName of the macro :")  ; ask for the name of the macro
  (kmacro-name-last-macro name)         ; use this name for the macro
  (find-file user-init-file)            ; open ~/.emacs or other user init file
  (goto-char (point-max))               ; go to the end of the .emacs
  (newline)                             ; insert a newline
  (insert-kbd-macro name)               ; copy the macro
  (newline)                             ; insert a newline
  (switch-to-buffer nil))               ; return to the initial buffer

;;------------------------------------------------------------------------------
;; Abbrev. Definitions

;; TODO: Transfer these to yasnippet
(setq dabbrev-case-replace nil)
(setq abbrev-mode t)

(define-abbrev-table 'java-mode-abbrev-table
  '(("sysout" "" sysout-skel 0)
    ("sysfor" "" sysfor-skel 0)
    ("cnc" "<code>null</code>" nil 1) ))

(define-skeleton sysout-skel
  "Java standard println statement"
  ""
  > "System.out.println(\"" _ "\");")

(define-skeleton sysfor-skel
  "Java standard println statement"
  ""
  > "System.out.format(\"" _ "%n\");")

(define-abbrev-table 'c-mode-abbrev-table
  '(("pnf" "" printf-skel 0)))

(define-skeleton printf-skel
  "C standard printf statement, with line and file macros."
  ""
  > "printf(\"%s:%d " _ "\\n\", __FILE__, __LINE__);")

(define-abbrev-table 'js2-mode-abbrev-table
  '(("clog" "" clog-skel 0)))

(define-skeleton clog-skel
  "Javascript standard console.log statement."
  ""
  > "console.log(\"" _ "\");")

;;------------------------------------------------------------------------------
;; Key binding overrides

(defvar miken-keys-minor-mode-map (make-keymap) "miken-keys-minor-mode keymap.")

(define-key miken-keys-minor-mode-map (kbd "M-n") 'miken-next-line-five)
(define-key miken-keys-minor-mode-map (kbd "M-p") 'miken-previous-line-five)

(define-minor-mode miken-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  nil " mn" miken-keys-minor-mode-map)

;;------------------------------------------------------------------------------
;; Server

(server-start)

;;------------------------------------------------------------------------------
;; Misc. custom keybindings

(global-set-key (kbd "C-x C-u") 'browse-url)
(global-set-key (kbd "C-c C-a") 'calendar)

(use-package xkcd :defer true)
(wg-revert-workgroup (wg-current-workgroup))

(global-set-key [S-return]
                (lambda () (interactive)
                  (end-of-line)
                  (newline-and-indent) ))

;;------------------------------------------------------------------------------
;; Exit

(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function kill-buffer-query-functions))

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (cl-flet ((process-list ())) ad-do-it))

;;------------------------------------------------------------------------------

(miken-lightsaber t)

;;; End .emacs
