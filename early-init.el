(setq package-enable-at-startup nil)

;; UI
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq frame-inhibit-implied-resize t)



;; FileNameHandler
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

;; GarbageCollection
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)
(add-hook 'after-init-hook
          `(lambda ()
             (setq gc-cons-threshold 67108864 ; 64mb
                   gc-cons-percentage 0.1
                   file-name-handler-alist file-name-handler-alist-original)
             (garbage-collect)) t)
