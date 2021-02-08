;;;
;;; Unused code snippets I want to keep around. Digging through git is a pain.
;;;

;;------------------------------------------------------------------------------
;; Global modes

(use-package discover :config (global-discover-mode 1))

;;------------------------------------------------------------------------------
;; Familiar key bindings

(global-set-key (kbd "<home>") #'move-beginning-of-line)
(global-set-key (kbd "<end>") #'move-end-of-line)
(global-set-key (kbd "<next>")
                (lambda () (interactive)
                  (scroll-up (miken-window-half-height))))
(global-set-key (kbd "<prior>")
                (lambda () (interactive)
                  (scroll-down (miken-window-half-height))))

;;------------------------------------------------------------------------------
;; Misc. custom keybindings

(global-set-key (kbd "C-c a") #'calendar)
(global-set-key [f9] #'save-buffer)

(defun miken-maximus-frame ()
  "Stretch a frame across two monitors"
  (interactive)
  (miken-font-size)
  (set-frame-position (selected-frame) 5 25)
  (set-frame-size (selected-frame) 237 93))
(global-set-key (kbd "C-M-s-f") #'miken-maximus-frame)

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

;;; End reference.el
