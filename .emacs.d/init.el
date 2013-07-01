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

                      ;; JavaScript
                      js2-mode
                      js2-refactor
                      ac-js2

                      ;; Ruby / Rails
                      bundler
                      haml-mode
                      rinari
                      rspec-mode
                      ruby-mode
                      ruby-tools
                      rvm
                      sass-mode
                      scss-mode
                      yaml-mode
                      yari

                      ;; Clojure
                      clojure-mode
                      hl-sexp
                      nrepl
                      nrepl-ritz
                      ac-nrepl

                      ;; Scala
                      scala-mode2

                      ;; Haskell
                      haskell-mode

                      ;; Python / Django
                      ein
                      jedi
                      epc

                      ;; R
                      ess

                      ;; Cucumber
                      feature-mode

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

;; inherit path modifications from the shell on Mac OS X
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; temporary files
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transform
      `((".*" ,temporary-file-directory t)))

;; ===========================================================================
;; General Settings
;; ===========================================================================
(when (memq window-system '(mac ns))
  (progn (add-to-list 'default-frame-alist '(height . 79))
         (add-to-list 'default-frame-alist '(width . 80))))

;; ===========================================================================
;; Color Theme
;; ===========================================================================
(require 'zenburn-theme)

;; ===========================================================================
;; Keybindings
;; ===========================================================================
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

(global-set-key (kbd "C-c n") 'esk-cleanup-buffer)

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
;; C / C++ / Objective C / Java / C#
;; ===========================================================================
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'pretty-mode)

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
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'hl-sexp-mode)

(setq nrepl-hide-special-buffers t)
(add-hook 'nrepl-mode-hook 'paredit-mode)
(add-hook 'nrepl-mode-hook 'hl-sexp-mode)
(add-hook 'nrepl-mode-hook
          (lambda ()
            (font-lock-mode)
            (clojure-mode-font-lock-setup)
            (font-lock-mode)))
(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)

(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))
(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook
          'set-auto-complete-as-completion-at-point-function)
(add-hook 'nrepl-mode-hook
          'set-auto-complete-as-completion-at-point-function)
(add-hook 'nrepl-interaction-mode-hook
          'set-auto-complete-as-completion-at-point-function)
(eval-after-load "nrepl"
  '(define-key nrepl-interaction-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc))

;; ===========================================================================
;; Scala
;; ===========================================================================
(let* ((ensime-load-path (concat vendor-base-load-path "/ensime/elisp"))
       (ensime-package   (concat ensime-load-path "/ensime.el")))
  (add-to-list 'load-path ensime-load-path)
  (when (file-exists-p ensime-package)
    (require 'ensime)
    (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
    (add-hook 'scala-mode-hook 'pretty-mode)))

(setq ensime-inf-cmd-template     '("sbt" "console" "-classpath" :classpath)
      ensime-inf-default-cmd-line '("sbt" "console"))

;; ===========================================================================
;; Ruby
;; ===========================================================================
(rvm-activate-corresponding-ruby)

;; use project-configured Ruby version
(add-hook 'ruby-mode-hook
          (lambda ()
            (rvm-activate-corresponding-ruby)))

;; TODO hack to suppress warning
(defalias 'inf-ruby-keys 'inf-ruby-setup-keybindings)

(add-hook 'ruby-mode-hook 'pretty-mode)

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
(add-hook 'python-mode-hook 'pretty-mode)

;; ===========================================================================
;; Haskell
;; ===========================================================================
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'pretty-mode)

;; ===========================================================================
;; Server
;; ===========================================================================
(server-start)
