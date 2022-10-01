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

    "p"   '(:ignore t :which-key "projectile")
    "pf"  'projectile-find-file
    "ps"  'projectile-ripgrep
    "pt"  'projectile-run-vterm))

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
  (setq evil-want-fine-undo t)
  (setq evil-undo-system 'undo-tree)
  :config
  (add-hook 'evil-mode-hook 'st/evil-hook)
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state))

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
  :custom ((magit-blame-echo-style 'margin)
           (magit-save-repository-buffers 'dontask))
  :init (global-set-key (kbd "C-c g") 'magit-status))

(use-package forge
  :after magit)
(use-package git-gutter
  :init (global-git-gutter-mode))

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)
(winner-mode)
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/dev")
    (setq projectile-project-search-path '("~/dev")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package swiper
  :bind ("C-c w" . swiper-isearch))

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

;; Scroll
(setq scroll-margin 0)
(setq scroll-conservatively 10000)
(setq scroll-preserve-screen-position t)

;; Automatically load changes to files
(global-auto-revert-mode t)

;; Font and ligatures
(set-frame-font "Iosevka 13" t)
(defun st/set-font-faces()
  (set-face-attribute 'default nil :font "Iosevka" :height 130)
  (set-face-attribute 'fixed-pitch nil :font "Iosevka" :height 130)
  (set-face-attribute 'variable-pitch nil :font "Aileron Light" :height 130 :weight 'regular))
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (st/set-font-faces)))))
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

(load-theme 'modus-vivendi)
(define-key global-map (kbd "<f5>") #'modus-themes-toggle)

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
(use-package tree-sitter
  :hook ('tree-sitter-after-on-hook . #'tree-sitter-hl-mode)
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))
(use-package tree-sitter-langs
  :after tree-sitter)

(use-package elixir-mode
  :custom (lsp-elixir-server-command '("elixir-ls"))
  :hook
  (elixir-mode . lsp-deferred)
  (elixir-mode . alchemist-mode))

(use-package elm-mode
  :hook (elm-mode . lsp-deferred))

(use-package haskell-mode
  :hook
  (haskell-literate-mode . lsp-deferred)
  (haskell-mode . lsp-deferred)
  (haskell-mode . haskell-indentation-mode)
  (haskell-mode . interactive-haskell-mode)
  :init
  (setq
   haskell-stylish-on-save t
   haskell-indentation-electric-flag t
   haskell-process-type 'cabal-repl
   haskell-interactive-popup-errors nil
   haskell-process-log t)
  :bind (:map haskell-mode-map
              ("C-c e" . haskell-process-load-file))
  :config
  (define-key interactive-haskell-mode-map (kbd "C-c C-l") nil)
  (add-to-list 'auto-mode-alist '("\\.cabal?\\'" . haskell-cabal-mode)))

(use-package lsp-haskell
  :custom (lsp-haskell-server-path "haskell-language-server-wrapper"))

(use-package python-mode
  :bind (:map python-mode-map ("C-c e" . run-python))
  :custom
  (python-shell-interpreter "ipython")
  (python-shell-interpreter-args "--pylab")
  (dap-python-executable "python3")
  :config
  (require 'dap-python))

(use-package python-black
  :after python
  :hook (python-mode . python-black-on-save-mode-enable-dwim))

(use-package py-isort
  :after python-black
  :hook (python-mode . py-isort-enable-on-save))

(use-package flycheck
  :config (setq-default flycheck-disabled-checkers '(python-pylint))
  :init (global-flycheck-mode))

(defun st/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . st/lsp-mode-setup)
  :bind (:map lsp-mode-map
              ("C-c C-l a" . lsp-execute-code-action)
              ("C-c C-l d" . lsp-ui-peek-find-definitions)
              ("C-c C-l r" . lsp-ui-peek-find-references))
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-enable-file-watchers nil)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq lsp-log-io nil)
  :config
  (advice-add 'lsp :before #'envrc-reload)
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :bind (:map lsp-ui-mode-map
              ("C-c i" . lsp-ui-imenu))

  :custom
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-sideline-show-code-actions nil))
(use-package lsp-treemacs
  :after lsp
  :commands lsp-treemacs-errors-list)

(use-package yasnippet
  :custom (yas-snippet-dirs '("~/.snippets/"))
  :init
  (yas-global-mode)
  (global-set-key (kbd "C-c y") 'company-yasnippet))

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html.eex?\\'" . web-mode))
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2))

(use-package css-mode
  :hook
  (css-mode . lsp)
  (scss-mode . lsp))

(use-package js2-mode
  :mode "\\.js[x]*$"
  :custom (js2-basic-offset 2)
  :hook (js2-mode . lsp-deferred))

(use-package typescript-mode
  :hook (typescript-mode . lsp-deferred)
  :after tree-sitter
  :config
  (setq typescript-indent-level 2)
  (define-derived-mode typescriptjsx-mode typescript-mode "Typescript jsx")
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptjsx-mode))
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptjsx-mode . tsx)))

(use-package company
  :after lsp-mode
  :hook
  (lsp-mode . company-mode)
  (emacs-lisp-mode . company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.2))
(global-set-key (kbd "TAB") #'company-indent-or-complete-common)
(use-package company-box
 :hook (company-mode . company-box-mode))

(use-package lsp-pyright
  :hook (python-mode . lsp-deferred))

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
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t) (emacs-lisp . t)))
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
  :bind ("C-c a" . org-agenda)
  :config (st/org-font-setup)
  :custom
  (org-confirm-babel-evaluate nil)
  (org-ellipsis " ▾")
  (org-hide-leading-stars nil)
  (org-agenda-files '("~/org/tasks.org" "~/org/calendar.org"))
  (org-agenda-prefix-format "%t %s")
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "IN-PROGRESS(i)" "|" "DONE(d1)")))
  (org-agenda-custom-commands
   '(("d" "Agenda dashboard view"
      ((agenda "" ((org-deadline-warning-days 7)))
       (todo "IN-PROGRESS"
             ((org-agenda-overriding-header "In-progress")
              (org-agenda-max-todos nil)))
       (todo "NEXT"
             ((org-agenda-overriding-header "Next Actions")
              (org-agenda-max-todos nil))))))))

(use-package org-capture
  :bind ("C-c c" . org-capture)
  :custom
  (org-capture-templates '(("t" "Task" entry
                            (file+headline "~/org/tasks.org" "Task list")
                            "* %^{Select status|TODO|NEXT|IN-PROGRESS}  %?"))))

(use-package org-superstar
  :hook (org-mode . (lambda () (org-superstar-mode 1))))

(use-package visual-fill-column
  :hook (org-mode . (lambda ()
                      (setq visual-fill-column-width 70
                            visual-fill-column-center-text t)
                      (visual-fill-column-mode 1))))
(use-package org-roam
  :init (setq org-roam-v2-ack t)
  :hook (org-mode . (lambda ()
                     (and (org-roam-file-p)
                          (not (eq 'visible (org-roam-buffer--visibility)))
                          (org-roam-buffer-toggle))))
  :custom
  (org-roam-directory "~/org/roam")
  (org-roam-completion-everywhere t)
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %<%T>:  %?"
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
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
  :bind (("C-c v" . persp-switch-to-buffer*)
         ("C-c V" . persp-switch)
         ("C-c s" . projectile-persp-switch-project))
  :custom (persp-mode-prefix-key (kbd "C-c C-v"))
  :config
  (unless persp-mode
    (persp-mode 1)))

(use-package nix-mode
  :hook (nix-mode . lsp-deferred)
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

(use-package popper
  :bind (("C-`"   . popper-toggle-latest)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :custom (popper-reference-buffers
           '("\\*Messages\\*"
             "Output\\*$"
             "\\*Async Shell Command\\*"
             "^\\*eshell.*\\*$" eshell-mode
             "^\\*vterm.*\\*$"  vterm-mode
             haskell-interactive-mode
             help-mode
             compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package smartparens
  :config (progn (show-smartparens-global-mode t))
  :hook ((elm-mode . turn-on-smartparens-strict-mode)
         (emacs-lisp-mode . turn-on-smartparens-strict-mode)
         (haskell-mode . turn-on-smartparens-strict-mode)
         (js2-mode . turn-on-smartparens-strict-mode)
         (typescript-mode . turn-on-smartparens-strict-mode)
         (nix-mode . turn-on-smartparens-strict-mode)
         (python-mode . turn-on-smartparens-strict-mode)))

(use-package zeal-at-point
  :bind ("C-c z" . zeal-at-point))

(use-package emacs
  :custom (compilation-scroll-output t))
(use-package sql
  :hook (sql-mode . lsp-deferred)
  :bind (:map sql-mode-map
              ("C-c e" . lsp-sql-execute-query)
              ("C-c s" . lsp-sql-switch-connection))
  :custom (lsp-sqls-workspace-config-path nil))
(use-package ace-window
  :bind ("C-x o" . ace-window))
(provide 'init)
;;; init.el ends here
