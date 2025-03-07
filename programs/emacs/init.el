(setq user-full-name "Sebastian Tunberg")
(setq user-mail-address "sebastian.tunberg@gmail.com")

(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

;; Clean buffers and no menu bars
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

;; Fix file uncrompression problems experienced when using Pgtk. credits: https://github.com/syl20bnr/spacemacs/issues/11585#issuecomment-1233440194
(auto-compression-mode 1)
;; Move betweeen windows using M-<left/right/up/down>
(windmove-default-keybindings 'meta)

(use-package emacs
  :custom
  (backup-by-copying t)
  (backup-directory-alist
   '(("." . "~/.saves/")))
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 2)
  (version-control t)
  (ignore-window-parameters t)
  (compilation-scroll-output t)
  (visible-bell t)
  :init
  (global-so-long-mode 1)
  (electric-pair-mode 1)
  (save-place-mode 1)
  (advice-add 'pop-to-mark-command :after #'recenter))

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

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package dired
  :custom (dired-kill-when-opening-new-dired-buffer t)
  :commands (dired dired-jump)
  :bind (:map dired-mode-map
              ("C-x C-j" . dired-jump)
              ("F" . dired-create-empty-file))


  :config
    (if (string= system-type "darwin")
        (setq dired-listing-switches "-aBhl")
        (setq dired-listing-switches "-agho --group-directories-first")))

(use-package magit
  :custom
  (magit-blame-echo-style 'margin)
  (magit-save-repository-buffers 'dontask)
  (magit-log-margin-show-committer-date t)
  :init
  (global-set-key (kbd "C-c g") 'magit-status))

(use-package forge
  :after magit)
(use-package git-gutter
  :init (global-git-gutter-mode))

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)
(winner-mode)
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

(use-package consult
  :bind (("C-c w"   . consult-line)
         ("C-c W"   . consult-focus-lines)
         ("C-x b"   . consult-buffer)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ("M-#"     . consult-register-load)
         ("M-'"     . consult-register-store)
         ("C-M-#"   . consult-register)
         ("M-s d"   . consult-find)
         ("M-s r"   . consult-ripgrep)))

(use-package vertico
  :init (vertico-mode))

(use-package vertico-posframe
  :init (vertico-posframe-mode 1)
  :custom (vertico-posframe-parameters '((left-fringe . 8)
                                         (right-fringe . 8))))
(use-package orderless
  :custom (completion-styles '(orderless)))

(use-package marginalia
  :init (marginalia-mode))

(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

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
  :config (setq which-key-idle-delay 1))

(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h C-d" . helpful-at-point)
         ("C-h F" . helpful-function)
         ("C-h C" . helpful-command))
  :custom
  (describe-function-function #'helpful-callable)
  (describe-variable-function #'helpful-variable))

(use-package dart-mode
  :hook (dart-mode . lsp-deferred))

(use-package treesit
  :init
  (setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (haskell "https://github.com/tree-sitter/tree-sitter-haskell")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
  :custom
  (treesit-font-lock-level 4)
  (major-mode-remap-alist
   '((bash-mode . bash-ts-mode)
     (css-mode . css-ts-mode)
     (dockerfile-mode . dockerfile-ts-mode)
     (json-mode . json-ts-mode)
     (python-mode . python-ts-mode)
     (tsx-mode . tsx-ts-mode)
     (typescript-mode . typescript-ts-mode)
     (yaml-mode . yaml-ts-mode))))

(use-package elixir-mode
  :custom (lsp-elixir-server-command '("elixir-ls"))
  :hook
  (elixir-mode . lsp-deferred)
  (elixir-mode . alchemist-mode))

(use-package elm-mode
  :hook (elm-mode . lsp-deferred))

(use-package haskell-ts-mode
  :config (setq haskell-ts-use-indent nil)
  :hook ((haskell-ts-mode . ormolu-format-on-save-mode)
          (haskell-ts-mode . eglot-ensure))
  :mode "\\.hs$")

(use-package eglot
  :bind (:map eglot-mode-map
              ("C-c l a" . eglot-code-actions)
              ("M-." . eglot-find-typeDefinition))
  :hook ((envrc-after-update-hook . eglot-reconnect)
         (envrc-after-update-hook . my/start-eglot-after-envrc))
  :config
  (add-to-list 'eglot-server-programs '(haskell-ts-mode . ("haskell-language-server" "--lsp")))
  :custom
  (eglot-autoshutdown t)
  (eglot-confirm-server-initiated-edits nil))

(use-package python-ts-mode
  :bind (:map python-ts-mode-map ("C-c c" . run-python))
  :custom
  (python-shell-interpreter "ipython")
  (python-shell-interpreter-args "-i --simple-prompt")
  (dap-python-debugger 'debugpy)
  (dap-python-executable "python3"))

(use-package dap-mode)

(use-package dap-python)

(use-package python-black
  :hook (python-ts-mode . python-black-on-save-mode-enable-dwim))

(use-package python-isort
  :hook (python-ts-mode . python-isort-on-save-mode))

(use-package flycheck
  :config (setq-default flycheck-disabled-checkers '(python-pylint python-mypy))
  :init (global-flycheck-mode))

(defun st/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.0)
  (corfu-quit-at-boundary 'separator)
  (corfu-echo-documentation 0.25)
  (corfu-preview-current 'insert)
  (corfu-preselect-first t)
  :bind (:map corfu-map
              ("M-SPC" . corfu-insert-separator)
              ("TAB" . corfu-insert)
              ("RET" . corfu-insert))
  :init
  (global-corfu-mode)
  (corfu-history-mode))

(use-package lsp-mode
  :custom
  (lsp-completion-provider :none)
  :commands (lsp lsp-deferred)
  :config
    (lsp-register-client
    (make-lsp-client :new-connection (lsp-stdio-connection "nixd")
                     :major-modes '(nix-mode)
                     :priority 0
                     :server-id 'nixd))
  :bind (:map lsp-mode-map
              ("C-c l a" . lsp-execute-code-action)
              ("M-." . lsp-find-definition)
              ("C-c l r" . lsp-ui-peek-find-references))
  :init
  (setq lsp-inline-completion-enable 't)
  (defun st/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(flex)))
  (setq lsp-keymap-prefix "C-c S-l")
  (setq lsp-enable-file-watchers nil)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq lsp-log-io nil)
  :hook
  (lsp-completion-mode . st/lsp-mode-setup-completion)
  (lsp-mode . st/lsp-mode-setup)
  :config
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
  :bind ("C-c y" . yas-insert-snippet)
  :init (yas-global-mode))


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

(use-package typescript-ts-mode
  :mode "\\.ts*$"
  :hook (typescript-ts-mode . lsp-deferred)
  :custom (typescript-indent-level 2))

(use-package tsx-ts-mode
  :mode "\\.tsx*$"
  :hook (tsx-ts-mode . lsp-deferred)
  :custom (typescript-indent-level 2))

(use-package prettier-js
  :hook (typescript-ts-base-mode . prettier-js-mode))

(use-package lsp-pyright
  :hook (python-ts-mode . lsp-deferred))

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
  :bind ("C-c o a" . org-agenda)
  :config
  (st/org-font-setup)
  (define-key org-mode-map (kbd "C-'") nil)
  :custom
  (org-agenda-span 'day)
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
  :bind ("C-c o c" . org-capture)
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
  :hook (org-roam-db-autosync-mode . (lambda ()
                     (and (org-roam-file-p)
                          (not (eq 'visible (org-roam-buffer--visibility)))
                          (org-roam-buffer-toggle))))
  :custom
  (org-roam-directory "~/org/roam")
  (org-roam-completion-everywhere t)
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %<%T>:  %?"
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
  :bind (("C-c o n l" . org-roam-buffer-toggle)
         ("C-c o n f" . org-roam-node-find)
         ("C-c o n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i" . completion-at-point))
  :bind-keymap
  ("C-c o n d" . org-roam-dailies-map)
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

(use-package nix-mode
  :hook (nix-mode . lsp-deferred)
  :mode "\\.nix\\'")

(use-package sh-mode
  :mode "zshrc\\'")

(use-package envrc
  :init (envrc-global-mode))

(use-package minions
  :config (minions-mode 1))

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
             "^\\*Python\\*$" inferior-python-mode
             "\\*Async Shell Command\\*"
             "^\\*eshell.*\\*$" eshell-mode
             "^\\*eat.*\\*$"  eat-mode
             flycheck-error-list-mode
             haskell-interactive-mode
             help-mode
             compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package devdocs
  :bind ("C-c z" . devdocs-lookup))
(use-package eat
  :bind ("C-c e" . eat))
(use-package sql
  :hook (sql-mode . lsp-deferred)
  :bind (:map sql-mode-map
              ("C-x C-e" . lsp-sql-execute-query))
  :custom (lsp-sqls-workspace-config-path nil))
(use-package ace-window
  :bind ("C-x o" . ace-window))
(use-package avy
  :bind
  ("C-'" . avy-goto-char-2)
  ("M-g w" . avy-goto-word-1))

(use-package pdf-tools
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page))

(use-package nov
  :config
    (add-to-list 'auto-mode-alist '("\\.epub?\\'" . nov-mode)))

(use-package syncorate
  :custom (syncorate-executable "~/.local/bin/syncorate"))

(use-package ligature
  :hook (prog-mode . ligature-mode)
  :config
    (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://")))

(use-package swarm-mode
  :mode "\\.sw*$"
  :hook (swarm-mode . lsp-deferred))

(use-package jsonnet-mode
  :after lsp-mode
  :init
  (defcustom lsp-jsonnet-executable "jsonnet-language-server"
    "Command to start the Jsonnet language server."
    :group 'lsp-jsonnet
    :risky t
    :type 'file)

  (add-to-list 'lsp-language-id-configuration '(jsonnet-mode . "jsonnet"))

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection (lambda () lsp-jsonnet-executable))
    :activation-fn (lsp-activate-on "jsonnet")
    :server-id 'jsonnet))
  :hook (jsonnet-mode . lsp-deferred))

(use-package combobulate
  :custom (combobulate-key-prefix "C-c u")
  :hook ((python-ts-mode . combobulate-mode)
         (js-ts-mode . combobulate-mode)
         (css-ts-mode . combobulate-mode)
         (yaml-ts-mode . combobulate-mode)
         (typescript-ts-mode . combobulate-mode)
         (tsx-ts-mode . combobulate-mode)))
(provide 'init)

;;; init.el ends here
