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
                      starter-kit
                      starter-kit-bindings
                      starter-kit-eshell
                      starter-kit-lisp
                      starter-kit-ruby
                      auto-complete
                      exec-path-from-shell
                      yasnippet
                      zenburn-theme

                      ;; HTML / CSS
                      rainbow-mode
                      css-eldoc

                      ;; JavaScript
                      js2-mode
                      js2-refactor
                      ac-js2
                      coffee-mode

                      ;; Ruby / Rails
                      bundler
                      enh-ruby-mode
                      haml-mode
                      rinari
                      rspec-mode
                      ruby-tools
                      rvm
                      sass-mode
                      scss-mode
                      slim-mode
                      yaml-mode
                      yari

                      ;; Clojure
                      clojure-mode
                      clojure-test-mode
                      hl-sexp
                      cider
                      ac-nrepl
                      slamhound

                      ;; Scala
                      scala-mode2
                      ensime

                      ;; Haskell
                      haskell-mode
                      ghc

                      ;; Racket
                      geiser

                      ;; Python / Django
                      ein
                      jedi
                      epc

                      ;; R
                      ess

                      ;; Cucumber
                      feature-mode

                      ;; LaTeX
                      auctex

                      ;; Miscellaneous
                      cl-lib
                      graphviz-dot-mode
                      lorem-ipsum
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
(when (memq window-system '(mac ns))
  (progn (add-to-list 'default-frame-alist '(height . 82))
         (add-to-list 'default-frame-alist '(width . 80))))

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
;; Keybindings
;; ===========================================================================
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c n")   'esk-cleanup-buffer)
(global-set-key (kbd "C-c =")   'er/expand-region)
(global-set-key (kbd "C-c a r") 'align-regexp)
(global-set-key (kbd "C-c t")   'toggle-transparency)

;; ===========================================================================
;; AutoComplete
;; ===========================================================================
(require 'auto-complete-config)
(ac-config-default)

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
(add-hook 'css-mode-hook 'paredit-mode)
(add-hook 'css-mode-hook 'rainbow-mode)

(setq scss-compile-at-save nil)

;; ===========================================================================
;; JavaScript
;; ===========================================================================
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
(customize-set-variable 'js2-basic-offset 2)

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

(add-hook 'lisp-mode-hook 'pretty-mode)

;; ===========================================================================
;; Clojure
;; ===========================================================================
(setq nrepl-hide-special-buffers t)
(setq cider-auto-select-error-buffer t)

(let* ((modes       '(paredit-mode hl-sexp-mode subword-mode))
       (cider-hooks '(cider-mode-hook cider-repl-mode-hook))
       (all-hooks   (cons 'clojure-mode-hook cider-hooks)))
  (dolist (hook all-hooks)
    (dolist (mode modes)
      (add-hook hook mode))
    (add-hook hook (lambda () (hl-line-mode -1))))
  (dolist (hook cider-hooks)
    (add-hook hook
              (lambda ()
                (font-lock-mode)
                (clojure-mode-font-lock-setup)
                (font-lock-mode)))))


;; ===========================================================================
;; Racket
;; ===========================================================================
(add-hook 'geiser-mode 'paredit-mode)

;; ===========================================================================
;; Scala
;; ===========================================================================
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(add-hook 'scala-mode-hook 'pretty-mode)

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
(rvm-activate-corresponding-ruby)

;; use project-configured Ruby version
(add-hook 'ruby-mode-hook
          (lambda ()
            (rvm-activate-corresponding-ruby)
            (rinari-launch)))

;; ===========================================================================
;; Python
;; ===========================================================================
(setq python-shell-interpreter      "ipython"
      python-shell-interpreter-args ""
      python-shell-prompt-regexp    "In \\[[0-9]+\\]:"
      python-shell-completion-setup-code
      "from IPython.core.completerlib import module_completion"
      python-shell-completion-module-string-code
      "';'.join(module_completion('''%s'''))\n"
      python-shell-completion-string-code
      "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'auto-complete-mode)
(add-hook 'python-mode-hook 'jedi:setup)

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

;; ===========================================================================
;; Server
;; ===========================================================================
(server-start)
