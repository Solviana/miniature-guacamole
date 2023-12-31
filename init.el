;;; init.el --- an ugly emacs config by Solviana
;;; Commentary:
;;; Dependencies to be installed from apt:
;;; fd-find (for projectile)
;;; pylint (for flymake)
;;; silversearcher-ag (for projectile)
;;; ripgrep (for projectile)
;;; elpa-elpy (for elpy)
;;; ccls
;;; Dependencies to be installed from pip:
;;; jedi flake8 autopep8 yapf black
;;; Code:
(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(set-face-attribute 'default nil :font "Monospace-10")
(global-display-line-numbers-mode)
(show-paren-mode)
(setq indent-tabs-mode nil)
(setq-default cmake-tab-width 4)
(setq-default truncate-lines t)
(set-face-attribute 'default nil :height 130)
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default c-basic-offset 4)
(setq require-final-newline t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(virtualenvwrapper eshell-prompt-extras yasnippet-snippets yasnipet treemacs-projectile treemacs ccls ag yaml-mode eww-lnum ace-window magit anaconda-mode company-jedi elpy undo-tree flycheck lsp-ui lsp-mode rustic rust-mode which-key use-package smartparens rg projectile monokai-theme counsel company cmake-mode))
 '(python-flymake-command '("pylint"))
 '(require-final-newline t)
 '(safe-local-variable-values
   '((vc-prepare-patches-separately)
     (diff-add-log-use-relative-names . t)
     (vc-git-annotate-switches . "-w"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; Configure package repositories
(require 'package)
(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; Install use-package if not already installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Load use-package
(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(setq use-package-always-ensure t)

;; Configure Monokai theme using use-package
(use-package monokai-theme
  :ensure t
  :config
  (load-theme 'monokai t))

(defun mark-symbol-at-point ()
  "Mark the entire symbol around the point."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (goto-char (car bounds))
      (push-mark (cdr bounds) nil t))))
(global-set-key (kbd "C-c m") 'mark-symbol-at-point)

(defun copy-symbol-at-point ()
  "Copy the symbol at point to the clipboard."
  (interactive)
  (let ((symbol (current-word)))
    (when symbol
      (kill-new symbol)
      (message "Copied symbol: %s" symbol))))
(global-set-key (kbd "C-c c") 'copy-symbol-at-point)
(global-set-key (kbd "C-c s") 'swiper-isearch-thing-at-point)

;; window manipulation

(defun resize-window-horizontally (direction)
    "Resize the window horizontally.
DIRECTION should be 1 to increase width, -1 to decrease."
    (interactive)
    (let ((npixels (if (eq direction -1) -5 5)))
      (if (window-resizable (selected-window) npixels 1)
	  (adjust-window-trailing-edge (selected-window) npixels 1))))

(defun my-resize-window-right ()
  "Increase the width of the current window by 10 pixels."
  (interactive)
  (resize-window-horizontally 1))

(defun my-resize-window-left ()
  "Decrease the width of the current window by 10 pixels."
  (interactive)
  (resize-window-horizontally -1))

(global-set-key (kbd "C-M-<right>") 'my-resize-window-right)
(global-set-key (kbd "C-M-<left>") 'my-resize-window-left)

;; Web browsing & search
(use-package eww
  :bind (("C-c w" . eww)) ; This sets a global key binding for EWW.
  :config
  ; You can set a default search engine here. This example uses Google.
  (setq eww-search-prefix "https://www.google.com/search?q="))

;; projectile for project management
(use-package projectile
  :ensure t
					;  :requires counsel
  :after counsel
  :diminish projectile-mode
  :config
  (setq projectile-project-search-path '("~/" "~/repos"))
  (setq projectile-enable-caching t)
; force indexing for gitignored folders (build, package caches etc...)
; this works only if fdfind is installed
  (setq-default projectile-git-fd-args "-H -0 --strip-cwd-prefix --no-ignore -E .git -tf -c never")
  (projectile-mode +1)
  :bind (("C-c p p"   . projectile-switch-project)
            ("C-c p f"   . projectile-find-file)
            ("C-c p s g" . projectile-grep)
            ("C-c p s r" . projectile-ripgrep)
	    ("C-c p s s" . projectile-ag)
            ("C-c p d"   . projectile-dired)
            ("C-c p c"   . projectile-compile-project)
            ("C-c p C"   . projectile-configure-project)
            ("C-c p u"   . projectile-run-project)
	    ("C-c p t"   . projectile-test-project)
	    ("C-c p r"   . projectile-find-references)
	    ("C-c p k"   . projectile-kill-buffers)
	    ("C-c p i"   . projectile-invalidate-cache)))

(use-package treemacs
  :ensure t
  :defer t
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-c t 1"   . treemacs-delete-other-windows)
        ("C-c t t"   . treemacs)
        ("C-c t d"   . treemacs-select-directory)
        ("C-c t B"   . treemacs-bookmark)
        ("C-c t C-t" . treemacs-find-file)
        ("C-c t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

;; ripgrep for searching
(use-package rg
  :ensure t)

;; ivy for autocompletion
(use-package counsel
  :ensure t
  :demand t
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d)")
  (setq projectile-completion-system 'ivy)
  :bind (("C-s"     . swiper-isearch)
            ("M-x"     . counsel-M-x)
            ("C-x C-f" . counsel-find-file)
            ("M-y"     . counsel-yank-pop)
            ("C-x b"   . ivy-switch-buffer)
            ("C-c v"   . ivy-push-view)
            ("C-c V"   . ivy-pop-view)
	    ("C-h f"   . counsel-describe-function)
	    ("C-h v"   . counsel-describe-variable)))

;; for pair management e.g. quotes, parentheses
(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (require 'smartparens-config) ; default config
  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1)
  (setq sp-show-pair-delay 0.1
        sp-show-pair-from-inside t
        sp-cancel-autoskip-on-backward-movement nil))

(use-package whitespace
  :ensure t
  :diminish global-whitespace-mode
  :custom
  (whitespace-style '(face tabs trailing space-before-tab space-after-tab empty indentation::space))
  :config
  (global-whitespace-mode))

(use-package ace-window
  :ensure t
  :bind (("C-x o" . ace-window)))

(use-package undo-tree
  :bind (("C-x u" . undo-tree-visualize)) ;; Set a keybinding for undo-tree-visualize
  :config
  (global-undo-tree-mode))

(use-package magit
  :ensure t
  :config
  ; we can remove the build in vc tool
  (keymap-global-unset "C-x v"))

(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.1)
  (setq which-key-side-window-max-height 0.33))

;; Org
(use-package yaml-mode
  :ensure t
  :mode ("\\.yaml\\'"))
;; Generic sw development packages
(use-package company
  :ensure t
  :diminish company-mode
  :config
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay 0.5)
  :hook
  (after-init . global-company-mode))

(use-package flycheck
  :hook (prog-mode . flycheck-mode))

;; C/C++ development
(use-package cmake-mode
  :ensure t
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(use-package ccls
  :ensure t
  :hook
  ((c-mode c++-mode objc-mode cuda-mode) . (lambda () (require 'ccls) (lsp)))
  :config
  (setq ccls-executable "/usr/bin/ccls"))

;; Rust
(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t))

;; Python
(use-package elpy
  :ensure t
  :init
  (elpy-enable)
  :config
  (setq flycheck-flake8-maximum-line-length 120)
  (remove-hook 'elpy-modules 'elpy-module-highlight-indentation) ; who thought this abberation is useful???
  (remove-hook 'elpy-modules 'elpy-module-flymake) ; might re-enable later if i figure out how to configure this...
)

(use-package company-jedi
  :ensure t
  :config
  (add-to-list 'company-backends 'company-jedi)
)

(use-package anaconda-mode
  :ensure t
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode)))

;; LSP and Snippets
(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook
  (lsp-mode . (lambda ()
                (let ((lsp-keymap-prefix "M-l"))
                  (lsp-enable-which-key-integration))))
  (lsp-mode . lsp-ui-mode)
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  ;; enable / disable the hints as you prefer:
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  :config
  (define-key lsp-mode-map (kbd "M-l") lsp-command-map)
  (keymap-set lsp-mode-map "C-c ?" 'lsp-ui-peek-find-references))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

(use-package yasnippet
  :ensure t
  :hook
  ((c-mode c++-mode python-mode) . yas-minor-mode))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;; Shell improvements
(use-package eshell-prompt-extras
  :ensure t
  :config
  (with-eval-after-load "esh-opt"
    ;; Load eshell-prompt-extras
    (require 'virtualenvwrapper)
    (autoload 'epe-theme-multiline-with-status "eshell-prompt-extras")
    ;; Set the prompt to use epe-theme, you can change the theme as desired
    (setq eshell-highlight-prompt nil
          eshell-prompt-function 'epe-theme-multiline-with-status)))

(provide 'init)
;;; init.el ends here
