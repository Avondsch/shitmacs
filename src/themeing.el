;;themeing for emacs


(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell nil)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook
		vterm-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(defvar efs/default-font-size 110)
(defvar efs/default-variable-font-size 110)


(set-face-attribute 'default nil :font "Scientifica" :height efs/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Scientifica" :height efs/default-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Scientifica" :height efs/default-variable-font-size)

(defun dw/replace-unicode-font-mapping (block-name old-font new-font)
  (let* ((block-idx (cl-position-if
                     (lambda (i) (string-equal (car i) block-name))
                     unicode-fonts-block-font-mapping))
         (block-fonts (cadr (nth block-idx unicode-fonts-block-font-mapping)))
         (updated-block (cl-substitute new-font old-font block-fonts :test 'string-equal)))
    (setf (cdr (nth block-idx unicode-fonts-block-font-mapping))
          `(,updated-block))))

(defun custo/setup-font-faces ()
  "Setup all customacs font faces."
  ;;re-disable GUI stuff we don't care about
  (push '(menu-bar-lines . 0) default-frame-alist)
  (push '(tool-bar-lines . 0) default-frame-alist)
  (push '(vertical-scroll-bars) default-frame-alist)
  (when (display-graphic-p)
    ;; set default font
    (set-face-attribute 'default nil :font (font-spec :family "Scientifica" :height efs/default-font-size))
    ;; Set the fixed pitch face
    (set-face-attribute 'fixed-pitch nil :font (font-spec :family "Scientifica" :height efs/default-font-size))
    ;; Set the variable pitch face which is the same for mac and linux
    (set-face-attribute 'variable-pitch nil :font (font-spec :family "Scientifica" :height efs/default-variable-font-size))
    ;; after org-mode we want to adjust font sizes
    (with-eval-after-load 'org
      (dolist (face '((org-level-1 . 1.3)
                      (org-level-2 . 1.25)
                      (org-level-3 . 1.20)
                      (org-level-4 . 1.15)
                      (org-level-5 . 1.10)
                      (org-level-6 . 1.05)
                      (org-level-7 . 1.0)
                      (org-level-8 . 1.0)))
        (set-face-attribute (car face) nil :font "Scientifica" :weight 'regular :height (cdr face))
        )

      ;; Ensure that anything that should be fixed-pitch in Org files appears that way
      (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
      (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
      (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
      (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
      (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
      (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
      (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
      )
    ;; set current frame to 120x45 characters
    (set-frame-width (frame-focus) 120)
    (set-frame-height (frame-focus) 45)))
;; run this hook after we have initialized the first time
(add-hook 'after-init-hook 'custo/setup-font-faces)
;; re-run this hook if we create a new frame from daemonized Emacs
(add-hook 'server-after-make-frame-hook 'custo/setup-font-faces)







(use-package unicode-fonts
    :straight t
    :config
    ;; Fix the font mappings to use the right emoji font
    (mapcar
     (lambda (block-name)
       (dw/replace-unicode-font-mapping block-name "Apple Color Emoji" "Noto Color Emoji"))
     '("Dingbats"
       "Emoticons"
       "Miscellaneous Symbols and Pictographs"
       "Transport and Map Symbols"))
    (unicode-fonts-setup))



;;theme and modeline

(add-to-list 'custom-theme-load-path "~/.config/emacs/extra-elisp/everforest-theme")
(use-package doom-themes
    :straight t
    :init(load-theme 'everforest-hard-dark))
(defun ap/load-doom-theme (theme)
  "Disable active themes and load a Doom theme."
  (interactive (list (intern (completing-read "Theme: "
                                              (->> (custom-available-themes)
                                                   (-map #'symbol-name)
                                                   (--select (string-prefix-p "doom-" it)))))))
  (ap/switch-theme theme)

  (set-face-foreground 'org-indent (face-background 'default)))

(defun ap/switch-theme (theme)
  "Disable active themes and load THEME."
  (interactive (list (intern (completing-read "Theme: "
                                              (->> (custom-available-themes)
                                                   (-map #'symbol-name))))))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme 'no-confirm))

(use-package all-the-icons
  :straight t)

(use-package mood-line
  :straight t
 :init (mood-line-mode))
;;transparency
  (set-frame-parameter (selected-frame) 'alpha '(100 . 100))
  (add-to-list 'default-frame-alist '(alpha . (100 . 100)))
