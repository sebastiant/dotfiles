(setq user-full-name "Sebastian Tunstig")
(setq user-mail-address "sebastian.tunstig@gmail.com")

(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

;; Clean buffers and no menu bars
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

;; Move betweeen windows using M-<left/right/up/down>
(windmove-default-keybindings 'meta)

(use-package no-littering
:config (setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
)
(setq create-lockfiles nil)
(use-package beacon
  :config
  (beacon-mode))

(use-package rainbow-delimiters
:commands (rainbow-delimiters-mode)
:init
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package general
  :after evil
  :config
  (general-override-mode)
  (general-define-key "C-h f" #'helpful-callable)
  (general-define-key "C-h v" #'helpful-variable)
  (general-define-key "C-h k" #'helpful-key)
  (general-define-key "C-h C-d" #'helpful-at-point)
  (general-define-key "C-h F" #'helpful-function)
  (general-define-key "C-h C" #'helpful-command)
  (general-create-definer tyrant-def
    :states '(normal visual insert motion emacs)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "C-SPC")
  (tyrant-def
    ""    nil

    "b"   '(:ignore t :which-key "buffer")
    "bb"  'persp-switch-to-buffer*
    "bd"  'persp-kill-buffer
    "bs"  'save-buffer

    "l"   '(:ignore t :which-key "lsp")
    "ll"  (general-simulate-key "C-c l")
    "ld"  'lsp-ui-peek-find-definitions
    "lr"  'lsp-ui-peek-find-references

    "v"   'persp-switch

    "f"   '(:ignore t :which-key "files")
    "ff"  'find-file

    "g"   '(:ignore t :which-key "magit")
    "gg"  'magit-status
    "gb"  'magit-blame

    "p"   '(:ignore t :which-key "projectile")
    "pp"  'projectile-persp-switch-project
    "pf"  'projectile-find-file
    "ps"  'projectile-ripgrep
    "pt"  'projectile-run-vterm

    "s"   'consult-line

    "w"   '(:ignore t :which-key "window")
    "ww"  'evil-window-next
    "wc"  'delete-window
    "wv"  'split-window-horizontally
    "ww"  'evil-window-next
    "ws"  'split-window-vertically

    "SPC" 'org-agenda))

(global-set-key (kbd "<escape>") 'keybaord-escape-quit)
(use-package undo-tree
  :init (global-undo-tree-mode t))

(defun st/evil-hook ()
  (dolist (mode '(custom-mode
		  eshell-mode
		  git-rebase-mode
		  term-mode
      vterm-mode
      ))
    (add-to-list 'evil-emacs-state-modes mode)))
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-fine-undo t)
  (setq evil-undo-system 'undo-tree)
  :config
  (add-hook 'evil-mode-hook 'st/evil-hook)
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-normal-state-map (kbd ", w") 'evil-window-vsplit))

(use-package evil-collection
  :after evil
  :custom (evil-collection-outline-bind-tab-p nil)
  :config (evil-collection-init))

(use-package dired
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))
(use-package dired-single
  :commands (dired dired-jump))

(use-package magit
  :custom (magit-blame-echo-style 'margin)
  :init (global-set-key (kbd "C-x g") 'magit-status))
(use-package forge
  :after magit)
(use-package git-gutter
  :init (global-git-gutter-mode))

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)
(winner-mode)
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

(use-package pyvenv
  :init (setenv "WORKON_HOME" "~/.pyenv/versions"))

(use-package py-isort
  :config (add-hook 'before-save-hook 'py-isort-before-save))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/dev")
    (setq projectile-project-search-path '("~/dev")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package vertico
  :init (vertico-mode))

(use-package orderless
  :custom (completion-styles '(orderless)))

(use-package marginalia
  :init (marginalia-mode))

(use-package embark
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s F" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch)
         :map isearch-mode-map
         ("M-e" . consult-isearch)                 ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch)               ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi))           ;; needed by consult-line to detect isearch

  ;; Enable automatic preview at point in the *Completions* buffer.
  ;; This is relevant when you use the default completion UI,
  ;; and not necessary for Vertico, Selectrum, etc.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-file consult--source-project-file consult--source-bookmark
   :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root))

(use-package embark-consult
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package savehist
  :init (savehist-mode))

;; Utf-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; macOS meta key
(setq mac-option-modifier 'meta)

;; Always follow symlinks
(setq vc-follow-symlinks t)

;; Line numbers
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

;; Scroll
(setq scroll-margin 0)
(setq scroll-conservatively 10000)
(setq scroll-preserve-screen-position t)

;; Automatically load changes to files
(global-auto-revert-mode t)

;; Font and ligatures
(set-frame-font "Iosevka 13" t)
(if (fboundp 'mac-auto-operator-composition-mode)
    (mac-auto-operator-composition-mode t))

;; Editing
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default evil-shift-width tab-width)

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

;; Trailing whitespace
(defun my/buf-show-trailing-whitespace ()
  (interactive)
    (setq show-trailing-whitespace t))
(add-hook 'prog-mode-hook 'my/buf-show-trailing-whitespace)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(trailing-whitespace ((t (:background "dim gray")))))

;; Highlight matching pairs of scope characters
(defvar show-paren-delay)
(setq show-paren-delay 0.0)
(show-paren-mode t)

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (load-theme 'doom-solarized-dark t)
  (doom-themes-visual-bell-config))

;; transparency
(set-frame-parameter (selected-frame) 'alpha '(96 . 80))
(add-to-list 'default-frame-alist '(alpha . (96 . 80)))

(require 'ansi-color)
(defun st/colorize-compilation-buffer ()
 (ansi-color-apply-on-region (point-min) (point-max)))
(add-hook 'compilation-filter-hook 'st/colorize-compilation-buffer)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config (setq which-key-idle-delay 1))

(use-package helpful
  :custom
  (describe-function-function #'helpful-callable)
  (describe-variable-function #'helpful-variable))

(use-package elixir-mode
  :hook (elixir-mode . lsp-deferred)
  :init (add-to-list 'exec-path "~/language-servers/elixir"))

(use-package haskell-mode
  :hook
  (haskell-literate-mode . lsp-deferred)
  (haskell-mode . lsp-deferred)
  (haskell-mode . haskell-indentation-mode)
  (haskell.mode . haskell-interactive-mode)
  :init
  (setq
   haskell-indentation-electric-flag t
   haskell-process-type 'cabal-repl
   haskell-interactive-popup-errors nil
   haskell-process-log t)
  :config
  (add-to-list 'auto-mode-alist '("\\.cabal?\\'" . haskell-mode)))

;; Python
(use-package python-mode
  :custom
  (python-shell-interpreter "python3")
  (dap-python-executable "python3")
  :config
  (require 'dap-python))
(defun st/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))
(use-package flycheck
  :config (setq-default flycheck-disabled-checkers '(python-pylint))
  :init (global-flycheck-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((lsp-mode . st/lsp-mode-setup)
         (python-mode . lsp))
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-enable-file-watchers nil)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq lsp-log-io nil)
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-sideline-show-code-actions nil))
(use-package lsp-treemacs
  :after lsp
  :commands lsp-treemacs-errors-list)

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html.eex?\\'" . web-mode))
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2))

(use-package js2-mode
  :mode "\\.js\\'"
  :hook (js2-mode . lsp-deferred))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  :bind (:map company-active-map
              ([return] . nil)
              ("ret"    . nil)
              ([tab]    . company-complete-selection)
              ("tab"    . company-complete-selection)))

(use-package company-box
 :hook (company-mode . company-box-mode))

(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred))))

;; Add buffer local Flycheck checkers after LSP for different major modes.
;; credits: https://github.com/flycheck/flycheck/issues/1762#issuecomment-749789589
(defvar-local my-flycheck-local-cache nil)
(defun my-flycheck-local-checker-get (fn checker property)
  ;; Only check the buffer local cache for the LSP checker, otherwise we get
  ;; infinite loops.
  (if (eq checker 'lsp)
      (or (alist-get property my-flycheck-local-cache)
          (funcall fn checker property))
    (funcall fn checker property)))
(advice-add 'flycheck-checker-get
            :around 'my-flycheck-local-checker-get)
(add-hook 'lsp-managed-mode-hook
          (lambda ()
            (when (derived-mode-p 'python-mode)
              (setq my-flycheck-local-cache '((next-checkers . (python-flake8)))))))
(add-hook 'lsp-managed-mode-hook
          (lambda ()
            (when (derived-mode-p 'haskell-mode)
              (setq my-flycheck-local-cache '((next-checkers . (haskell-hlint)))))))
(add-hook 'lsp-managed-mode-hook
          (lambda ()
            (when (derived-mode-p 'sh-mode)
              (setq my-flycheck-local-cache '((next-checkers . (sh-shellcheck)))))))


(defun st/org-mode-setup ()
  (visual-line-mode 1)
  (org-indent-mode)
  (variable-pitch-mode 1)
  (display-line-numbers-mode 0))

(defun st/org-font-setup ()
  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Aileron Light" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(defun st/org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of A, B or C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))

(use-package org
  :hook (org-mode . st/org-mode-setup)
  :config (st/org-font-setup)
  :custom
  (org-ellipsis " ▾")
  (org-hide-leading-stars nil)
  (org-agenda-files '("~/org" "~/org/roam" "~/org/roam/daily"))
  (org-agenda-prefix-format "%t %s")
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "IN-PROGRESS(i)" "|" "DONE(d1)")))
  (org-agenda-custom-commands
   '(("d" "Agenda dashboard view"
      ((agenda "" ((org-deadline-warning-days 7)))
       (tags-todo "+PRIORITY=\"A\""
                  ((org-agenda-overriding-header "High Priority")))
       (todo "NEXT"
             ((org-agenda-overriding-header "Next Actions")
              (org-agenda-max-todos nil)))
       (todo "TODO"
             ((org-agenda-overriding-header "Backlog")
              (org-agenda-skip-function
                  '(or (st/org-skip-subtree-if-priority ?A)
                       (org-agenda-skip-if nil '(scheduled deadline)))))))))))
(use-package org-superstar
  :hook (org-mode . (lambda () (org-superstar-mode 1))))

(use-package visual-fill-column
  :hook (org-mode . (lambda ()
                      (setq visual-fill-column-width 70
                            visual-fill-column-center-text t)
                      (visual-fill-column-mode 1))))
(use-package org-roam
  :hook (org-roam-find-file . (lambda () (persp-switch "roam")))
  :init (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/org/roam")
  (org-roam-completion-everywhere t)
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %<%T>:  %?"
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n")
      )
     ("t" "todo" entry "* TODO  %?"
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n")
      )))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i" . completion-at-point))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies)
  (org-roam-db-autosync-mode))

(use-package org-tree-slide
  :hook ((org-tree-slide-play . (lambda () (hide-mode-line-mode 1)
                                            (org-display-inline-images)
                                            (setq text-scale-mode-amount 2)
                                            (text-scale-mode 1)))
         (org-tree-slide-stop . (lambda () (text-scale-mode 0)
                                            (hide-mode-line-mode 0))))
  :custom
  (org-tree-slide-slide-in-effect t)
  (org-tree-slide-header 1)
  (org-tree-slide-breadcrumbs " > ")
  (org-image-actual-width nil))

(use-package perspective
  :config
  (unless persp-mode
    (persp-mode 1)))

(use-package nix-mode
  :mode "\\.nix\\'")
(use-package sh-mode
  :mode "zshrc\\'")

(use-package envrc
  :init (envrc-global-mode))

(use-package minions
  :config (minions-mode 1))

(use-package elfeed
  :config (setq elfeed-feeds '("https://hnrss.org/best")))

(use-package editorconfig
  :config (editorconfig-mode 1))

(use-package terraform-mode
  :hook (terraform-mode . terraform-format-on-save-mode))

(provide 'init)
;;; init.el ends here
