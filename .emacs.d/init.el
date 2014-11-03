;; ===========================================================================
;; package.el
;; ===========================================================================
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(;; Core
                      auto-complete
                      company
                      exec-path-from-shell
                      ido-ubiquitous
                      paredit
                      smex
                      yasnippet
                      zenburn-theme

                      ;; JavaScript
                      js2-mode
                      coffee-mode
                      js-comint

                      ;; Clojure
                      clojure-mode
                      hl-sexp
                      cider

                      ;; Racket
                      geiser

                      ;; Haskell
                      haskell-mode
                      ghc
                      shm
                      company-ghc

                      ;; OCaml
                      tuareg

                      ;; Scala
                      scala-mode2
                      ensime

                      ;; Erlang
                      erlang

                      ;; Go
                      go-mode
                      go-autocomplete

                      ;; LaTeX
                      auctex

                      ;; Markdown
                      markdown-mode

                      ;; Miscellaneous
                      expand-region
                      pretty-mode))

(dolist (package my-packages)
  (when (not (package-installed-p package))
    (package-install package)))

;; ===========================================================================
;; Paths
;; ===========================================================================
;; Vendor (non-ELPA/MELPA) packages
(setq vendor-base-load-path (concat (getenv "HOME") "/.emacs.d/vendor"))

;; inherit path modifications from the shell
(exec-path-from-shell-initialize)

;; temporary files
(setq temporary-file-directory "/tmp/emacs")
(when (not (file-exists-p temporary-file-directory))
  (mkdir temporary-file-directory))
(setq
 auto-save-file-name-transform `((".*" ,temporary-file-directory t))
 backup-by-copying             t
 backup-directory-alist        `((".*" . ,temporary-file-directory))
 delete-old-versions           t
 kept-new-versions             5
 kept-old-versions             2
 version-control               t)

;; ===========================================================================
;; General Settings
;; ===========================================================================
(setq inhibit-startup-screen t)
(setq initial-scratch-message
      "\"If the old fairy-tale ending \'They lived happily ever after\' is taken
to mean \'They felt for the next fifty years exactly as they felt the
day before they were married,' then it says what probably never was
nor ever would be true, and would be highly undesirable if it were.
Who could bear to live in that excitement for even five years? What
would become of your work, your appetite, your sleep, your
friendships? But, of course, ceasing to be \'in love\' need not mean
ceasing to love.

Love in this second sense-love as distinct from \'being in love\'—is not
merely a feeling. It is a deep unity, maintained by the will and
deliberately strengthened by habit; reinforced by (in Christian
marriages the grace which both partners ask, and receive, from God.)

They can have this love for each other even at those moments when they
do not like each other; as you love yourself even when you do not like
yourself.

They can retain this love even when each would easily, if they allowed
themselves, be \'in love\' with someone else. \'Being in love' first
moved them to promise fidelity: this quieter love enables them to keep
the promise. It is on this love that the engine of marriage is run:
being in love was the explosion that started it.\"

;; C.S. Lewis

(setq vows-to-laura
      '((I promise to be transparent in everything I say and everything I do.)
        (I promise to find new and creative ways to fulfill your needs and
           express my love for you everyday.)
        (I promise to always aspire to new goals together with you, and
           consistently act to progress toward those goals.)
        (I promise to cultivate our relationship to grow in all directions, to
           fill the spaces that are empty in order to strengthen the bond
           between us.)
        (I promise to always heed your counsel and maintain an open atmosphere
           of accountability.)))

(setq vows-to-kids
      '((We promise to be your advocates. We will stand up for you.)
        (We promise to provide a peaceful, caring home.)
        (We promise to give you room to grow. We will celebrate your successes
            with you and extend grace to learn by failure.)
        (We promise to \'train you up in the way you should go, so that when you
            are old you do not depart from it.\')
        (We promise that as our family grows and changes, our love for you and
            our commitment to you will grow as well.)))
")

(when (memq window-system '(mac ns))
  (progn (add-to-list 'default-frame-alist '(height . 82))
         (add-to-list 'default-frame-alist '(width . 80))))

(ido-mode t)
(setq ido-enable-flex-matching t)

(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(global-hl-sexp-mode)

(setq-default indent-tabs-mode nil)

(add-hook 'after-init-hook 'global-company-mode)

;; ===========================================================================
;; Display Settings
;; ===========================================================================
(require 'zenburn-theme)
;; (require 'solarized-light-theme)
;; (require 'solarized-dark-theme)

(defun toggle-transparency ()
  (interactive)
  (let ((param (cadr (frame-parameter nil 'alpha))))
    (if (and param (/= param 100))
        (set-frame-parameter nil 'alpha '(100 100))
      (set-frame-parameter nil 'alpha '(85 50)))))

;; ===========================================================================
;; Utilities
;; ===========================================================================

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

;; ===========================================================================
;; Keybindings
;; ===========================================================================
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

(global-set-key (kbd "C-s")         'isearch-forward-regexp)
(global-set-key (kbd "C-r")         'isearch-backward-regexp)
(global-set-key (kbd "C-M-s")       'isearch-forward)
(global-set-key (kbd "C-M-r")       'isearch-backward)
(global-set-key (kbd "M-x")         'smex)
(global-set-key (kbd "M-X")         'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(global-set-key (kbd "C-x C-b")     'ibuffer)
(global-set-key (kbd "C-c q")       'join-line)
(global-set-key (kbd "C-=")         'er/expand-region)
(global-set-key (kbd "C-c a r")     'align-regexp)
(global-set-key (kbd "C-c C-n b")   'cleanup-buffer)
(global-set-key (kbd "C-c t")       'toggle-transparency)

;; ===========================================================================
;; Company
;; ===========================================================================

;; ===========================================================================
;; yasnippet
;; ===========================================================================
(setq yas/load-directory "~/.emacs.d/vendor/snippets")

;; ===========================================================================
;; Org
;; ===========================================================================
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (clojure    . t)
   (lisp       . t)
   (ruby       . t)
   (python     . t)
   (java       . t)
   (js         . t)
   (R          . t)))

;; ===========================================================================
;; C / C++ / Objective C / Java / C#
;; ===========================================================================
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'pretty-mode)

;; ===========================================================================
;; CSS / Sass / SCSS
;; ===========================================================================

;; ===========================================================================
;; JavaScript
;; ===========================================================================
(add-hook 'inferior-js-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
(add-to-list 'comint-preoutput-filter-functions
             (lambda (output)
               (replace-regexp-in-string "\033\\[[0-9]+[GJK]" "" output)))

(add-hook 'js2-mode-hook
          (lambda ()
            (smartparens-mode)
            (pretty-mode)
            (local-set-key (kbd "C-x C-e") 'js-send-last-sexp)
            (local-set-key (kbd "C-M-x")   'js-send-last-sexp-and-go)
            (local-set-key (kbd "C-c b")   'js-send-buffer-and-go)
            (local-set-key (kbd "C-c l")   'js-load-file-and-go)))

(custom-set-variables
 '(js2-basic-offset 2))

;; ===========================================================================
;; Elisp
;; ===========================================================================
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

;; ===========================================================================
;; Common Lisp / SLIME
;; ===========================================================================
(defun slime ()
  (interactive)
  (fmakunbound 'slime)
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  (set-language-environment "UTF-8")
  (setq slime-net-coding-system 'utf-8-unix)
  (setq inferior-lisp-program "clisp")
  (slime))

(add-hook 'lisp-mode-hook 'paredit-mode)
(add-hook 'lisp-mode-hook 'pretty-mode)

;; ===========================================================================
;; Clojure
;; ===========================================================================
(setq nrepl-hide-special-buffers t)
(setq cider-auto-select-error-buffer t)

(let* ((modes       '(paredit-mode show-paren-mode hl-sexp-mode subword-mode))
       (cider-hooks '(cider-mode-hook cider-repl-mode-hook))
       (all-hooks   (cons 'clojure-mode-hook cider-hooks)))
  (dolist (hook all-hooks)
    (dolist (mode modes)
      (add-hook hook mode))
    (add-hook hook (lambda () (hl-line-mode -1)))))

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;; ===========================================================================
;; Racket
;; ===========================================================================

;; ===========================================================================
;; Erlang
;; ===========================================================================
(let* ((erlang-version "17.1_1")

       (erlang-path      
	(concat "/usr/local/Cellar/erlang/" erlang-version))

       (erlang-load-path 
	(concat erlang-path "/lib/erlang/lib/tools-2.6.15/emacs")))

  (add-to-list 'load-path erlang-load-path))
(require 'erlang-start)

(setq erlang-indent-level      2)
(setq erlang-electric-commands '(erlang-electric-semicolon))

;; ===========================================================================
;; Scala
;; ===========================================================================
(setq scala-mode2-load-path
      (concat vendor-base-load-path "/scala-mode2"))
(add-to-list 'load-path scala-mode2-load-path)
(setq ensime-load-path
      (concat vendor-base-load-path "/ensime"))
(add-to-list 'load-path ensime-load-path)
(require 'scala-mode2)
(require 'ensime)

(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(add-hook 'ensime-mode-hook
          (lambda ()
            (progn
              (local-set-key (kbd "C-c C-v C-e d") 'ensime-inf-eval-definition)
              (local-set-key (kbd "C-c C-v C-e r") 'ensime-inf-eval-region)
              (local-set-key (kbd "C-c C-v C-e b") 'ensime-inf-eval-buffer))))

(setq ensime-inf-cmd-template     '("sbt" "console" "-classpath" :classpath)
      ensime-inf-default-cmd-line '("sbt" "console"))

;; ===========================================================================
;; Ruby
;; ===========================================================================
;; (rvm-activate-corresponding-ruby)

;; use project-configured Ruby version
;; (add-hook 'ruby-mode-hook
;;           (lambda ()
;;             (rvm-activate-corresponding-ruby)
;;             (rinari-launch)))

;; ===========================================================================
;; Python
;; ===========================================================================
;; (setq python-shell-interpreter      "ipython"
;;       python-shell-interpreter-args ""
;;       python-shell-prompt-regexp    "In \\[[0-9]+\\]:"
;;       python-shell-completion-setup-code
;;       "from IPython.core.completerlib import module_completion"
;;       python-shell-completion-module-string-code
;;       "';'.join(module_completion('''%s'''))\n"
;;       python-shell-completion-string-code
;;       "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

;; (autoload 'jedi:setup "jedi" nil t)
;; (add-hook 'python-mode-hook 'auto-complete-mode)
;; (add-hook 'python-mode-hook 'jedi:setup)

;; ===========================================================================
;; Haskell
;; ===========================================================================
(setq shm-load-path
      (concat vendor-base-load-path "/structured-haskell-mode/elisp"))
(add-to-list 'load-path shm-load-path)
(require 'shm)

(add-hook 'haskell-mode-hook (lambda () (ghc-init)))
(add-hook 'haskell-mode-hook 'structured-haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'pretty-mode)
(add-hook 'haskell-mode-hook (lambda () (hl-line-mode -1)))
(add-hook 'haskell-mode-hook 'company-mode)

(add-hook 'ghc-mod (lambda ()
                     (add-to-list 'company-backends 'company-ghc)))

;; ===========================================================================
;; Coq
;; ===========================================================================
;; (setq auto-mode-alist (cons '("\\.v$" . coq-mode) auto-mode-alist))
;; (autoload 'coq-mode "coq" "Major mode for editing Coq vernacular." t)

;; ===========================================================================
;; Server
;; ===========================================================================
(server-start)
