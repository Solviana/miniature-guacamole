;;; init.el --- an ugly emacs config by Solviana
;;; Commentary:
;;; Required emacs features (emacs has to be compiled with this):
;;; svg support (for modeline + treemacs)
;;; xml support (for eww)
;;; lijansson (for lsp speed)
;;; native compilation (https://www.masteringemacs.org/article/speed-up-emacs-libjansson-native-elisp-compilation)
;;;
;;; Dependencies to be installed from apt:
;;; fd-find (for projectile)
;;; pylint (for flymake)
;;; silversearcher-ag (for projectile)
;;; ripgrep (for projectile)
;;; elpa-elpy (for elpy)
;;; ccls (g++-10)
;;; Fantasque Sans Mono
;;;
;;; Dependencies to be installed from pip:
;;; jedi flake8 autopep8 yapf black cppman
;;;
;;; Man + CPP integration:
;;; `cppman -s cppreference.com -c`
;;; `cppman -m true`
;;; Find the created man pages and add them to MANPATH
;;; run mandb
;;;
;;; Code:

(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(set-face-attribute 'default nil :font "Fantasque Sans Mono")
(show-paren-mode)
(display-time-mode)
(setq indent-tabs-mode nil)
(setq-default cmake-tab-width 4)
(setq-default truncate-lines nil)
(set-face-attribute 'default nil :height 170)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default c-basic-offset 4)
(setq require-final-newline t)
(setq initial-major-mode 'org-mode)
(setq tab-always-indent 'complete)
(winner-mode)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq gc-cons-threshold 100000000)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-scroll-output 'first-error)
 '(org-duration-format '(("h") (special . h:mm)))
 '(package-selected-packages
   '(breadcrumb zoom doom-modeline treemacs-magit all-the-icons doom-themes ox-extra ox-latex gptel expand-region virtualenvwrapper eshell-prompt-extras yasnippet-snippets yasnipet treemacs-projectile treemacs ccls ag yaml-mode eww-lnum ace-window magit anaconda-mode company-jedi elpy undo-tree flycheck lsp-ui lsp-mode rustic rust-mode which-key use-package smartparens rg projectile monokai-theme counsel company cmake-mode))
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

(use-package zoom
  :ensure t
  :after which-key
  :hook (after-init . zoom-mode)
  :custom
  (zoom-size '(0.618 . 0.618))
  (zoom-ignored-major-modes '(which-key-mode))
  :config
  ;; ignore which key -> does not work completely TODO
  (setq zoom-ignore-predicates
	'((lambda ()
	    (equal which-key-buffer-name
		   (buffer-file-name (current-buffer)))))))

;; nice command buffer in the middle of the screen
(use-package vertico-posframe
  :ensure t
  :after vertico
  :init (vertico-posframe-mode 1)
  :custom
  (vertico-posframe-min-width 150)
  (vertico-posframe-parameters '((left-fringe . 8)
                                 (right-fringe . 8))))

(use-package ace-window
  :ensure t
  :bind (("C-x o"   . ace-window)
         ("C-x C-o" . ace-swap-window)))

(use-package expand-region
  :bind ("C-c m" . er/expand-region))

;; Web browsing & search
(use-package eww
  :bind (("C-c w" . eww)) ; This sets a global key binding for EWW.
  :config
  ; You can set a default search engine here. This example uses Google.
  (setq eww-search-prefix "https://www.google.com/search?q="))

;; Remote access
(use-package tramp
  :ensure t)

;; projectile for project management
;; TODO: check if project.el can fulfill the same functionality
;; e.g.: configurable find file command to index gitignored files as well
(use-package projectile
  :ensure t
  :after consult
  :diminish projectile-mode
  :demand t
  :config
  (setq projectile-project-search-path '("~/" "~/repos"))
  (setq projectile-enable-caching t)
; force indexing for gitignored folders (build, package caches etc...)
; this works only if fdfind is installed
  (setq-default projectile-git-fd-args "-H -0 --strip-cwd-prefix --no-ignore -E .git -tf -c never")
  (projectile-mode +1)
  :bind    (("C-c p s g" . projectile-grep)
            ("C-c p s r" . projectile-ripgrep)
            ("C-c p s s" . projectile-ag) ; search commands moved to consul and M-s
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
  :hook (after-init . treemacs)
  :config
  (treemacs-resize-ui (treemacs-directory-face treemacs-directory-collapsed-face treemacs-file-face treemacs-root-face
		       treemacs-root-unreadable-face treemacs-root-remote-face treemacs-root-remote-unreadable-face
		       treemacs-root-remote-disconnected-face treemacs-tags-face treemacs-help-title-face
		       treemacs-help-column-face treemacs-term-node-face treemacs-on-success-pulse-face treemacs-on-failure-pulse-face
		       treemacs-marked-file-face treemacs-fringe-indicator-face treemacs-header-button-face treemacs-git-commit-diff-face
		       treemacs-git-ignored-face treemacs-git-added-face treemacs-git-modified-face treemacs-git-unmodified-face treemacs-git-untracked-face)
		      0.9)
  (setq treemacs-width 30)
  (treemacs-project-follow-mode 1)
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

;; minibuffer/completion
;; source: https://github.com/minad/vertico
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :custom
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Emacs 28 and newer: Hide commands in M-x which do not work in the current
  ;; mode.  Vertico commands are hidden in normal buffers. This setting is
  ;; useful beyond Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init
  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; Replace I-search with consult-line
	 ("C-s" . consult-line)
	 ;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ;("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ;; ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
)

(use-package consult-projectile
  :ensure t
  :after projectile
  :config
  :bind    (("C-c p p"   . consult-projectile-switch-project)
            ("C-c p f"   . consult-projectile-find-file)
            ("C-c p d"   . consult-projectile-find-dir)))

;; for pair management e.g. quotes, parentheses
(use-package smartparens
  :ensure t
  :demand t
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
  (which-key-add-key-based-replacements "C-x n" "narrow")
  (which-key-add-key-based-replacements "C-c &" "yasnippet"))

;; Org-mode setup
(use-package org
  :ensure t
  :bind (("C-c a o" . org-agenda))
  :config
  (require 'ox-latex)
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)
  (setq org-todo-keywords
      '((sequence "TODO" "ONGOING" "WAITING" "|" "DONE" "CANCELLED")))
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
  (setq company-idle-delay 0.25)
  :hook
  (after-init . global-company-mode))

;; (use-package flycheck
;;   :hook (prog-mode . flycheck-mode))

;; use flymake as it integrates with eglot out of the box
(use-package flymake
  :bind (("C-c ! l" . flymake-show-buffer-diagnostics)
	 ("C-c ! d" . flymake-show-project-diagnostics)
	 ("C-c ! n" . flymake-goto-next-error)
	 ("C-c ! p" . flymake-goto-prev-error)
	 ("C-c ! s" . consult-flymake)))

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

(use-package breadcrumb
  :ensure t
  :hook (prog-mode . breadcrumb-mode))

;; C/C++ development
(use-package cmake-mode
  :ensure t
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

;; Python development
(use-package pyvenv
  :ensure t)

(use-package poetry
  :ensure t)

;; Groovy
(use-package groovy-mode
  :ensure t
  :mode "\\.groovy\\'")

(use-package yasnippet
  :ensure t
  :demand t
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;; Shell improvements
(use-package ansi-color
  :ensure t
  :demand t
  :config
  (defun my-colorize-compilation-buffer ()
  (when (eq major-mode 'compilation-mode)
    (ansi-color-apply-on-region compilation-filter-start (point-max))))

  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))


(use-package eshell-prompt-extras
  :ensure t
  :config
  ;; handle ansi escape codes... thi probably belongs somewhere else
  ;; ROOM FOR IMPROVEMENT
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
  (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
  (add-hook 'eshell-preoutput-filter-functions
            'ansi-color-filter-apply)
  (add-hook 'eshell-preoutput-filter-functions
            'ansi-color-apply)
  (with-eval-after-load "esh-opt"
    ;; Load eshell-prompt-extras
    (require 'virtualenvwrapper)
    (autoload 'epe-theme-multiline-with-status "eshell-prompt-extras")
    ;; Set the prompt to use epe-theme, you can change the theme as desired
    (setq eshell-highlight-prompt nil
          eshell-prompt-function 'epe-theme-multiline-with-status)))

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
  :config
  (add-hook 'markdown-mode (lambda () (setq truncate-lines nil))))

(provide 'init)
;;; init.el ends here
