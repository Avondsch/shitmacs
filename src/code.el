;;IDE configurations for emacs.


;;No littering
;;(use-package no-littering)
;;(setq auto-save-file-name-transforms
;;    `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))


;; Ivy, and Company mode
(use-package ivy
    :straight t
    :diminish
    :bind (("C-s" . swiper)
           :map ivy-minibuffer-map
           ("TAB" . ivy-alt-done)
           ("C-l" . ivy-alt-done)
           ("C-j" . ivy-next-line)
           ("C-k" . ivy-previous-line)
           :map ivy-switch-buffer-map
           ("C-k" . ivy-previous-line)
           ("C-l" . ivy-done)
           ("C-d" . ivy-switch-buffer-kill)
           :map ivy-reverse-i-search-map
           ("C-k" . ivy-previous-line)
           ("C-d" . ivy-reverse-i-search-kill))
    :config
    (ivy-mode 1))

(use-package ivy-rich
:straight t
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package counsel
:straight t
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

(use-package ivy-prescient
:straight t
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  ;; Uncomment the following line to have sorting remembered across sessions!
  ;(prescient-persist-mode 1)
  (ivy-prescient-mode 1))

;;elisp and lisp editing

(use-package lispy
    :straight t
    :hook ((emacs-lisp-mode . lispy-mode)
           (lisp-mode . lispy-mode))
    :config
    (add-hook hook #'lispy-mode)
    (add-hook 'lispy-mode-hook #'show-paren-mode)
    (add-hook 'lispy-mode-hook #'electric-pair-local-mode)
    (add-hook 'lispy-mode-hook #'eldoc-mode)
    (add-hook 'lispy-mode-hook #'lispyville-mode)
    (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
    (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode))

(with-eval-after-load 'lispy
  (lispy-set-key-theme '(lispy c-digits)))

(add-to-list 'auto-mode-alist '("\\.el\\'" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("\\.lisp\\'" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))

(use-package paredit
  :straight t
  :hook (emacs-lisp-mode . paredit-mode)
  (lisp-mode . paredit-mode)
 :config
  (add-hook 'paredit-mode-hook 'show-paren-mode)
  (add-hook 'paredit-mode-hook 'electric-pair-mode)
  (add-hook 'paredit-mode-hook 'eldoc-mode))

(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

;; Enable eldoc-mode for showing function signatures in minibuffer
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

;; Indentation settings
(setq-default lisp-body-indent 2
              lisp-indent-function 'common-lisp-indent-function)

;; Enable rainbow-delimiters mode for visualizing nested parentheses
(use-package rainbow-delimiters
  :straight t
  :hook(emacs-lisp-mode . rainbow-delimiters-mode)
            (lisp-mode . rainbow-delimiters-mode))

;; Enable aggressive-indent mode for automatic indentation
(use-package aggressive-indent
  :straight t
  :hook
  (emacs-lisp-mode . aggressive-indent-mode)
  (lisp-mode . aggressive-indent-mode)
  :config
  (add-to-list 'aggressive-indent-excluded-modes 'lisp-interaction-mode))

;; keybindings for evaluation and interaction
(define-key emacs-lisp-mode-map (kbd "C-c C-e") 'eval-last-sexp)
(define-key emacs-lisp-mode-map (kbd "C-c C-r") 'eval-region)

;;-------------------------------------------------------------------------------------------------------------------------------

;;lsp-mode, company, tree-sitter and language specific configs.

(use-package lsp-mode
    :straight t
    :commands lsp
    :config
    ;; Set up LSP server executables
    (setq lsp-clients-java-server-executable "<jdt-language-server>")
    ;; (setq lsp-clojure-server-command '("<clojure-lsp>")
    ;;     lsp-clojure-custom-server-command '("<path-to-clojure-language-server>"))
    ;; lsp-clojure-server-config-files ["<path-to-clojure-lsp-config.edn>"]
    (setq lsp-rust-server 'rust-analyzer)
    (setq lsp-nix-executable "<rnix-lsp>")
    (setq lsp-erlang-server '("erlang-ls")
	  (setq lsp-cpp-server 'clangd)))

  (use-package lsp-ui
    :straight t
    :hook (lsp-mode . lsp-ui-mode))

(use-package company
    :straight t
    :hook (lsp-mode . company-mode)
    :config
    (setq company-minimum-prefix-length 1
          company-idle-delay 0.0))
(use-package flycheck
    :straight t
    :hook (lsp-mode . flycheck-mode))

(use-package tree-sitter
    :straight t
    :config
    (require 'tree-sitter-langs) 
    (require 'tree-sitter-java)
    (require 'tree-sitter-clojure) 
    (require 'tree-sitter-rust)
    (require 'tree-sitter-nix) 
    (require 'tree-sitter-php))


;; for Java
(use-package lsp-java
    :straight t)

(add-to-list 'auto-mode-alist '("\\.java\\'" . java-mode))
(use-package java-mode
    :hook (java-mode . lsp-deferred)
    (java-mode . company-mode)
    (java-mode . flycheck-mode)
    :config
    (setq lsp-java-eldoc-enable-hover nil)
    (setq lsp-java-signature-help-enabled nil)
    (setq lsp-java-spring-boot-enabled nil)
    (setq lsp-java-save-actions-organize-imports nil) 
    (setq c-basic-offset 2
	  tab-width 2
	  indent-tabs-mode 1))
(use-package gradle-mode
    :load-path "/home/avondsch/.config/emacs/extra-elisp/emacs-gradle-mode/"
    :ensure nil
    :config
    (gradle-mode 1)
    (defun my-java-mode-hook ()
      (gradle-mode 1))
    (defun my-java-mode-exit-hook ()
      (gradle-mode -1))
    (add-hook 'java-mode-hook 'my-java-mode-hook)
    (add-hook 'java-mode-hook 'my-java-mode-exit-hook))

;; for C/C++
(add-to-list 'auto-mode-alist '("\\.c\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c-mode))

(use-package c-mode
    :hook (c-mode . lsp-deferred)
    (c-mode . tree-sitter-mode)
    (c-mode . company-mode)
    (c-mode . flycheck-mode)
    :config
    (setq c-default-style "gnu") 
    (setq c-basic-offset 4) ; Set the default indentation level to 4 spaces
    (setq tab-width 4)	    ; Set the default tab width to 4 spaces
    (electric-pair-mode 1)
    (setq c-electric-flag t)
    (setq c-electric-brace-placement 'always)
    (c-toggle-hungry-state 1)
    (subword-mode 1)
    (define-key c-mode-base-map (kbd "RET") 'newline-and-indent) ; Insert a newline and indent
    (define-key c-mode-base-map (kbd "C-c C-f") 'ff-find-other-file) ; Switch between header/source files
    (define-key c-mode-base-map (kbd "C-c C-c") 'comment-or-uncomment-region))



;;projectile, magit, treemacs

(use-package projectile
    :straight t
    :init
    (setq projectile-switch-project-action #'projectile-dired)
    :config
    (projectile-mode +1)
    (setq projectile-completion-system 'ivy)
    (setq projectile-enable-caching t)
    (setq projectile-indexing-method 'alien)
    (setq projectile-project-search-path '("~/code"))
    (setq projectile-globally-ignored-directories
          '(".git" ".svn" "node_modules" "dist" "build"))
    (setq projectile-globally-ignored-files
          '(".DS_Store" "*.jpg" "*.png" "*.gif" "*.pdf"))
    (setq projectile-ignored-file-extensions
          '("class" "pyc" "elc" "o"))
    :bind-keymap
    ("C-c p" . projectile-command-map))

(use-package counsel-projectile
    :straight t
    :config
    (counsel-projectile-mode))
(use-package projectile-git-autofetch
    :straight t
    :config
    (add-hook 'projectile-mode-hook 'projectile-git-autofetch-mode))


(use-package magit
    :straight t
    :bind (("C-x g" . magit-status)
           ("C-c g" . magit-file-dispatch))
    :config
    (setq forge-alist
	  '(("github.com" "api.github.com" "github.com" forge-github-repository)))
    (setq auth-sources '("~/.config/emacs/authinfo.gpg"))

    ;; Set the default action for commit to 'commit-and-push'
    (setq magit-push-always-verify nil)
    (setq magit-commit-ask-to-stage t)

    ;; Define additional keybindings
    (define-key magit-mode-map "j" 'magit-section-forward)
    (define-key magit-mode-map "k" 'magit-section-backward)

    ;; Customize log view
    (setq magit-log-arguments '("--graph" "--decorate" "--color"))

    ;; Show word-granularity differences in hunk headers
    (setq magit-diff-refine-hunk 'all)

    ;; Set the default upstream remote to 'origin'
    (setq magit-default-tracking-name-function 'magit-default-tracking-name-branch-only)

    ;; Show recent branches first in branch selection
    (setq magit-list-refs-sortby "-committerdate")

    ;; Enable blame mode
    (setq magit-blame-styles
          '((margin
             (margin-format " %s%f" " %C %a" " %C %H")
             (margin-width . 42)
             (margin-face . magit-blame-margin)
             (margin-body-face magit-blame-dimmed))
            (headings
             (heading-format "%-20a %C %s" "%-20a %C %s" "%-20a %C %s")
             (heading-face . magit-blame-heading)
             (heading-body-face . magit-blame-heading)
             (heading-margin-face . magit-blame-heading)))
          magit-blame-heading-format-function 'magit-blame-heading-format-function-two-lines)
    (set-face-attribute 'magit-diff-added nil :foreground "green4")
    (set-face-attribute 'magit-diff-removed nil :foreground "red3")

    ;; Enable Magit Forge for integrating with Git hosting platforms
    (use-package forge
	:straight t
	:after magit)

    ;; Enable Magit Gitflow for Git flow integration
    (use-package magit-gitflow
	:straight t
	:after magit
	:hook (magit-mode . turn-on-magit-gitflow))

    ;; Enable Magit-gh-pulls for GitHub pull request integration
    (use-package magit-gh-pulls
	:straight t
	:after magit
	:config
	(add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls))

    ;; Enable Magit-Todos for highlighting TODOs in commits
    (use-package magit-todos
	:straight t
	:after magit
	:hook (magit-mode . magit-todos-mode)))

;;sublimity
(use-package sublimity
    :straight t)
