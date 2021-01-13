(setq user-full-name "Sebastian Tunstig")
(setq user-email-address "sebastian.tunstig@gmail.com")

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-always-ensure t)

;; Clean buffers and no menu bars
(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq initial-scratch-message nil)

(use-package ivy
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

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         ("C-M-j" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))
;; magit
(use-package magit
  :init (global-set-key (kbd "C-x g") 'magit-status))

;; Rainbow-delimeters
(use-package rainbow-delimiters
:commands (rainbow-delimiters-mode)
:init
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Evil-mode
(use-package evil
  :init
  (setq evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode)
  (define-key evil-normal-state-map (kbd ", w") 'evil-window-vsplit))

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
(set-frame-font "Iosevka 15" t)
(if (fboundp 'mac-auto-operator-composition-mode)
    (mac-auto-operator-composition-mode t))

;; highlight trailing whitespace
(defun my/buf-show-trailing-whitespace ()
  (interactive)
    (setq show-trailing-whitespace t))
(add-hook 'prog-mode-hook 'my/buf-show-trailing-whitespace)
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

;; Theme
(use-package color-theme-sanityinc-solarized
  :init (load-theme 'sanityinc-solarized-dark t))

;; Telephone Line
(use-package telephone-line
  :init
    (setq telephone-line-lhs
	  '((evil   . (telephone-line-evil-tag-segment))
	    (accent . (telephone-line-vc-segment
		       telephone-line-erc-modified-channels-segment
		       telephone-line-process-segment))
	    (nil    . (telephone-line-minor-mode-segment
		       telephone-line-buffer-segment))))
    (setq telephone-line-rhs
	  '((nil    . (telephone-line-misc-info-segment))
	    (accent . (telephone-line-major-mode-segment))
	    (evil   . (telephone-line-airline-position-segment))))
  :config
  (telephone-line-mode 1))

;; Haskell
(use-package haskell-mode)
(use-package hindent
  :init (add-hook 'haskell-mode-hook #'hindent-mode))

;; yaml
(use-package yaml-mode)

;; Docker
(use-package dockerfile-mode)

(custom-set-variables
 '(package-selected-packages
   '(dockerfile-mode yaml-mode hindent haskell-mode telephone-line color-theme-sanityinc-solarized evil rainbow-delimiters magit helm use-package)))
