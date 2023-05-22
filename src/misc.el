;;for extra configuration that doesnt necessarily fall into themeing, ide, org-mode, and evil-mode configuration.

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

;;elcord
(use-package elcord
	     :straight t
	     :init (elcord-mode))

;;ibuffer
(use-package ibuffer
  :straight t
  :init (setq ibuffer-mode t)
  :config
 (setq ibuffer-saved-filter-groups
      '(("main"
         ("modified" (and
                      (modified . t)
                      (visiting-file . t)))
         ("code" (or (filename . "/projects/")
                     (filename . "/git-repos/")
                     (filename . "/code/")))
        ;; ("org" (mode . org-mode))
         ("dired" (mode . dired-mode))
         ("help" (or (name . "\*Help\*")
                     (name . "\*Apropos\*")
                     (name . "\*info\*")
                     (mode . help-mode)))
         ("internal" (name . "^\*.*$"))
         ("other" (name . "^.*$")))))

  (add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "main")))

  :bind
  (("<leader> b b" . ibuffer)))

(use-package all-the-icons-ibuffer
  :straight t
  :ensure t
  :init (all-the-icons-ibuffer-mode))

;;perspective.el

;;eshell

(defun efs/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Bind some useful keys for evil-mode
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-normalize-keymaps)

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt
:straight t
  :after eshell)

(use-package eshell
:straight t
  :hook (eshell-first-time-mode . efs/configure-eshell)
  :config

  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim")))

  (eshell-git-prompt-use-theme 'powerline))

;;vterm
(use-package vterm
:straight t
 :commands vterm
 :bind
 (("<leader> t o" . vterm)) 

 :config
  ;; Set the maximum scrollback buffer size (adjust as needed)
  (setq vterm-max-scrollback 10000)

  ;; Set the font size and face for vterm
  (custom-set-faces
   '(vterm ((t (:inherit default :family "Scientifica" :height 90))))))
  (custom-set-faces '(vterm-buffer-name ((t (:foreground "white" :background "black" :left-margin 8 :right-margin 8)))))

;;smooth scrolling

