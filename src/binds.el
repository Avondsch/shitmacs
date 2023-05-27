;;keybindings
;;Evil (emacs vi layer)

(setq evil-want-keybinding nil)
(use-package evil
   :straight t
   :init
  (evil-mode 1)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  (evil-set-leader 'normal (kbd "SPC"))
  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
    :straight t
    :after evil
    :config
    (evil-collection-init))


;;general keybinds

(use-package cua
    :init (cua-mode t)
    :bind (("<leader> n c" . cua-copy-region)
           ("<leader> n p" . cua-paste)))
(use-package eval
  :bind (("<leader> h r r" . eval-buffer))) ;;doom-emacs style baby!

(global-set-key (kbd "<leader> b k") 'kill-buffer)


;;terminal things
(use-package vterm
    :straight t
    :bind (("<leader> t p" . term-paste)
	   ("<leader> t c" . vterm-copy-mode)))

(use-package comint
  :bind (:map comint-mode-map
         ("<leader> t k" . comint-interrupt-subjob)))
