;;; init.el --- an ugly emacs config by Solviana
;;; Commentary:
;;; Dependencies to be installed from apt:
;;; fd-find (for projectile)
;;; pylint (for flymake)
;;; silversearcher-ag (for projectile)
;;; ripgrep (for projectile)
;;; elpa-elpy (for elpy)
;;; ccls (g++-10)
;;; Fantasque Sans Mono
;;; Dependencies to be installed from pip:
;;; jedi flake8 autopep8 yapf black
;;; Code:
(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(set-face-attribute 'default nil :font "Fantasque Sans Mono")
(global-display-line-numbers-mode)
(show-paren-mode)
(display-time-mode)
(setq indent-tabs-mode nil)
(setq-default cmake-tab-width 4)
(setq-default truncate-lines t)
(set-face-attribute 'default nil :height 120)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default c-basic-offset 4)
(setq require-final-newline t)
(setq initial-major-mode 'org-mode)
(setq tab-always-indent 'complete)
(winner-mode)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq gc-cons-threshold 100000000)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(doom-modeline treemacs-magit all-the-icons doom-themes ox-extra ox-latex gptel expand-region virtualenvwrapper eshell-prompt-extras yasnippet-snippets yasnipet treemacs-projectile treemacs ccls ag yaml-mode eww-lnum ace-window magit anaconda-mode company-jedi elpy undo-tree flycheck lsp-ui lsp-mode rustic rust-mode which-key use-package smartparens rg projectile monokai-theme counsel company cmake-mode))
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

;; UI improvements
(use-package all-the-icons
  :ensure t)

(use-package nerd-icons
  :ensure t)

(use-package doom-themes
  :ensure t
  :after treemacs
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-city-lights t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package dimmer
  :ensure t
  :config
  (dimmer-configure-which-key)
  (dimmer-configure-helm)
  (dimmer-mode t)
  (setq dimmer-fraction 0.3))

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

(use-package expand-region
  :bind ("C-c m" . er/expand-region))

;; Web browsing & search
(use-package eww
  :bind (("C-c w" . eww)) ; This sets a global key binding for EWW.
  :config
  ; You can set a default search engine here. This example uses Google.
  (setq eww-search-prefix "https://www.google.com/search?q="))

;; projectile for project management
(use-package projectile
  :ensure t
  :after counsel
  :diminish projectile-mode
  :demand t
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

(defmacro treemacs-resize-ui (faces size)
  "Resizes all faces to size"
    `(progn
     ,@(mapcar (lambda (face)
                 `(set-face-attribute ',face nil :height ,size))
               faces)))

(use-package treemacs
  :ensure t
  :demand t
  :bind
  (:map global-map
        ("C-c t 1"   . treemacs-delete-other-windows)
        ("C-c t t"   . treemacs)
        ("C-c t d"   . treemacs-select-directory)
        ("C-c t B"   . treemacs-bookmark)
        ("C-c t C-t" . treemacs-find-file)
        ("C-c t M-t" . treemacs-find-tag)
	("C-0". treemacs-select-window))
  :config
  (treemacs-resize-ui (treemacs-directory-face treemacs-directory-collapsed-face treemacs-file-face treemacs-root-face
		       treemacs-root-unreadable-face treemacs-root-remote-face treemacs-root-remote-unreadable-face
		       treemacs-root-remote-disconnected-face treemacs-tags-face treemacs-help-title-face
		       treemacs-help-column-face treemacs-term-node-face treemacs-on-success-pulse-face treemacs-on-failure-pulse-face
		       treemacs-marked-file-face treemacs-fringe-indicator-face treemacs-header-button-face treemacs-git-commit-diff-face treemacs-git-ignored-face)
		      0.9)
  (add-hook 'treemacs-mode-hook (lambda () (display-line-numbers-mode -1)))
  (setq treemacs-width 30)
  (treemacs)
  (treemacs-project-follow-mode 1)
  (treemacs-tag-follow-mode 1)
  (setq treemacs-tag-follow-delay 0.2)
  (setq treemacs-file-follow-delay 0.2))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :demand t
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :demand t
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
        sp-cancel-autoskip-on-backward-movement nil)
  :bind (:map prog-mode-map
              ("M-<right>" . sp-forward-slurp-sexp)
              ("M-<left>" . sp-forward-barf-sexp)))

(use-package whitespace
  :ensure t
  :diminish global-whitespace-mode
  :custom
  (whitespace-style '(face tabs trailing space-before-tab space-after-tab empty indentation::space))
  :config
  (global-whitespace-mode))

(use-package ace-window
  :ensure t
  :bind (("C-x o" . ace-window)
	 ("C-x C-o" . ace-swap-window)))

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
  :demand t
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.1)
  (setq which-key-side-window-max-height 0.33)
  (which-key-add-key-based-replacements "C-c p" "projectile")
  (which-key-add-key-based-replacements "C-c a" "org-agenda")
  (which-key-add-key-based-replacements "C-c t" "treemacs")
  (which-key-add-keymap-based-replacements prog-mode-map "C-c !" "linter")
  (which-key-add-key-based-replacements "C-c g" "ChatGPT")
  (which-key-add-key-based-replacements "C-x n" "narrow"))

;; Org-mode setup
(use-package org
  :ensure t
  :bind (("C-c a o" . org-agenda))
  :config
  (require 'ox-latex)
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)
  (defun org-sum-subtask-effort ()
  "Sum the efforts of all subtasks and set the effort of the parent task."
  (interactive)
  (save-excursion
    (when (org-before-first-heading-p) (error "Not inside a heading"))
    (let ((total-minutes 0))
      (org-up-heading-safe)
      (org-map-entries
       (lambda ()
         (let ((effort (org-entry-get nil "Effort")))
           (when effort
             (let ((minutes (org-duration-to-minutes effort)))
               (setq total-minutes (+ total-minutes minutes))))))
       "Effort>\"\"" 'tree)
      (org-set-property "Effort"
                        (org-minutes-to-clocksum-string total-minutes)))))
  :custom
  (org-agenda-files '("~/proj/todo"))
  (org-agenda-window-setup 'only-window)
  (org-agenda-restore-windows-after-quit t))

(use-package org-contrib
  :ensure t
  :config
  (require 'ox-extra)
  (ox-extras-activate '(latex-header-blocks ignore-headlines)))

;; LaTeX and PDF export setup with AUCTeX
(use-package auctex
  :ensure t
  :defer t)

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

;; Language server
;; https://github.com/MaskRay/ccls/wiki/FAQ#some-cc-headers-are-not-recognized
;; you have to check if the std include paths are included in the search path of clang
;; How to do this? `clang -v -fsyntax-only -x c++ /dev/null`
(use-package eglot
  :ensure t
  :config
  (defun eglot-ccls-inheritance-hierarchy (&optional derived)
    "Show inheritance hierarchy for the thing at point.
If DERIVED is non-nil (interactively, with prefix argument), show
the children of class at point."
    (interactive "P")
    (if-let* ((res (jsonrpc-request
                    (eglot--current-server-or-lose)
                    :$ccls/inheritance
                    (append (eglot--TextDocumentPositionParams)
                            `(:derived ,(if derived t :json-false))
                            '(:levels 100) '(:hierarchy t))))
              (tree (list (cons 0 res))))
	(with-help-window "*ccls inheritance*"
          (with-current-buffer standard-output
            (while tree
              (pcase-let ((`(,depth . ,node) (pop tree)))
		(cl-destructuring-bind (&key uri range) (plist-get node :location)
                  (insert (make-string depth ?\ ) (plist-get node :name) "\n")
                  (make-text-button (+ (point-at-bol 0) depth) (point-at-eol 0)
                                    'action `(lambda (_arg)
                                               (interactive)
                                               (find-file (eglot--uri-to-path ',uri))
                                               (goto-char (car (eglot--range-region ',range)))))
                  (cl-loop for child across (plist-get node :children)
                           do (push (cons (1+ depth) child) tree)))))))
      (eglot--error "Hierarchy unavailable"))))

;; C/C++ development
(use-package cmake-mode
  :ensure t
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))


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

;; Groovy
(use-package groovy-mode
  :ensure t
  :mode "\\.groovy\\'")

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

(defcustom gptel-maximum-column-width 100
  "Maximum width of chatgpt responses."
  :type 'integer
  :group 'gptel)

;; AI chatbots
(use-package gptel
  :ensure t
  :custom
  (gptel-model "gpt-4-1106-preview")
  :bind
  (("C-c g t" . gptel)
   ("C-c g s" . gptel-send)
   ("C-c g m" . gptel-menu)
   ("C-c g a" . gptel-abort))
  :config)

(provide 'init)
;;; init.el ends here
