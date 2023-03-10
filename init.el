(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(setq make-backup-files nil)
(setq auto-save-default nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(setq inhibit-startup-message t)

(scroll-bar-mode -1)  ; Oculta la barra de desplazamiento
(tool-bar-mode -1)    ; Oculta la barra de herramientas
(tooltip-mode -1)     ; Oculta en menu de opciones
(set-fringe-mode 10)  ; Añade un poco de espacio a los costados de la pantalla

(menu-bar-mode -1)    ; Desactiva en menu

(setq visible-bell t) ; Notificaion visual de campana

;; Desactiva números de linea para ciertos modos
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                markdown-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Añade números de linea relativos
(column-number-mode)
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
       (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(straight-use-package 'org)

;; Evita tener que escribir ":straight t" cada vez que se llama use-package
(setq straight-use-package-by-default t)

(use-package mixed-pitch
  :hook
  (text-mode . mixed-pitch-mode))
;; Esto define el tamaño de fuenta global
(defvar angl/default-font-size 125)

(set-face-attribute 'default nil :family "Iosevka" :height angl/default-font-size)
(set-face-attribute 'fixed-pitch nil :family "Iosevka" :height angl/default-font-size)
(set-face-attribute 'variable-pitch nil :family "Iosevka Comfy Duo" :height angl/default-font-size :weight 'regular)

(org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)))

(dolist (hook '(text-mode-hook))
      (add-hook hook (lambda () (flyspell-mode 1))))
    (setq ispell-program-name "hunspell")
    (setq ispell-dictionary "es_AR")

(use-package flyspell-correct
    :after flyspell
    :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

  (use-package flyspell-correct-ivy
    :after flyspell-correct)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.2))

(use-package helpful
  :straight t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package general
  :after evil
  :config
  (general-create-definer angl/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
   ;; Formato general para combinaciones. Mas al final.
  (angl/leader-keys
   "v" '(:ignore t :which-key "Alternar")
   "vt" '(counsel-load-theme :which-key "Elejir Tema")))
(general-define-key
 ;; Usa esto para alternar entre buffers
 "C-M-j" 'counsel-switch-buffer)

;; Añade iconos para diferentes cosas
(use-package all-the-icons
  :straight t
  :if (display-graphic-p))

;; Como 70 temas diferentes
(use-package doom-themes)

;; Diferencia visual entre buffers reales y temporales
(use-package solaire-mode)
(solaire-global-mode +1)

;; Para mejor diferencias las parentesis
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Sobresalta indentamiento
(use-package highlight-indent-guides
  :custom
  (highlight-indent-guides-delay 0)
  (highlight-indent-guides-responsive t)
  (highlight-indent-guides-method 'character)
  ;; (highlight-indent-guides-auto-enabled t)
  ;; (highlight-indent-guides-character ?\┆)
  :commands highlight-indent-guides-mode
  :hook (prog-mode  . highlight-indent-guides-mode))

;; La linea de modos usada por Doom Emacs
(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode)
  :custom ((doom-modeline-height 35)))

;; Termite tomar capturas de pantallas personalizadas dentro de Emacs en la region seleccionada
(straight-use-package
 '(screenshot :type git :host github :repo "tecosaur/screenshot"))

;; Configura cual tema usar (recomiendo siempre usar los proveidos por "doom-themes")
(load-theme 'doom-solarized-light :no-confirm)

(use-package emojify
  :hook (after-init . global-emojify-mode))
(add-hook 'after-init-hook #'global-emojify-mode)

(use-package dashboard
  :ensure t
  :after all-the-icons
  :init (add-hook 'dashboard-mode-hook (lambda () (setq show-trailing-whitespace nil)))
  :custom
  (dashboard-set-navigator t)
  (dashboard-center-content t)
  (dashboard-set-file-icons t)
  (dashboard-set-heading-icons t)
  (dashboard-image-banner-max-height 250)
  (dashboard-banner-logo-title "[PAX VOBISCUM]")
  (dashboard-startup-banner (concat user-emacs-directory "imagenes/angel.png"))
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (setq initial-buffer-choice (lambda () (dashboard-refresh-buffer)(get-buffer "*dashboard*")))
  (setq dashboard-footer-icon (all-the-icons-octicon "calendar"
                                                     :height 1.1
                                                     :v-adjust -0.05
                                                     :face 'font-lock-keyword-face))

  (setq
   dashboard-projects-backend 'project-el
   dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name
   dashboard-items '((recents        . 5)
                     (agenda         . 3)
                     (projects       . 2)))
  :custom-face
  (dashboard-heading ((t (:foreground nil :weight bold)))))

(use-package writeroom-mode)

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
         :map minibuffer-local-map
         ("C-r" . 'counsel-buffer-history))
  :config
  (setq ivy-initial-inputs-alist nil))
(use-package ivy-rich
:init
(ivy-rich-mode 1))

(use-package flx
  :after ivy
  :defer t
  :init
  (setq ivy-flx-limit 10000))

(use-package drag-stuff
  :hook ((prog-mode org-mode) . drag-stuff-mode )
  :bind
  ("C-M-S-j" . drag-stuff-down)
  ("C-M-S-k" . drag-stuff-up))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

;; Varias integraciones adicionales para ciertos modos
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package hydra
  :defer t)

(defhydra hydra-text-scale (:timeout 1)
  "tamaño del texto"
  ("j" text-scale-increase "acercar")
  ("k" text-scale-decrease "alejar")
  ("f" nil "salir" :exit t))

(angl/leader-keys
  "ts" '(hydra-text-scale/body :which-key "tamaño del texto"))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Programing") ;; Cambiar al tuyo
    (setq projectile-project-search-path '("~/Programing")))
  (setq projectile-switch-project-action #'project-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :bind (:map magit-status-mode-map
              ("c" . magit-commit-create)))

(use-package undo-tree
  :delight
  :bind ("C-x u" . undo-tree-visualize)
  :hook (org-mode . undo-tree-mode)
  :init (global-undo-tree-mode)
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-history-directory-alist '(("." . "~/.emacs.d/var/undo-tree-hist")))
  (undo-tree-visualizer-timestamps t))

(use-package dired
  :straight nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))

(use-package dired-single
  :commands (dired dired-jump))

(use-package all-the-icons-dired
  :if (display-graphic-p)
  :hook (dired-mode . all-the-icons-dired-mode)
  :config (setq all-the-icons-dired-monochrome nil))

(use-package dired-open
  :commands (dired dired-jump)
  :config
  (setq dired-open-extensions '(("png" . "feh")
                                ("mkv" . "mpv"))))

(use-package treemacs)
(use-package lsp-treemacs
  :after lsp)
(use-package treemacs-evil
  :after (treemacs evil)
  :straight t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :straight t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :straight t)
(add-hook 'dired-mode-hook 'treemacs-icons-dired-mode)
(use-package treemacs-magit
  :after (treemacs magit)
  :straight t)

(defun angl/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . angl/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  ;; Archivos que se relacionan con la agenda
  (setq org-agenda-files
        '("~/Org/Haceres.org"
          "~/Org/Cumpleaños.org"
          "~/Org/Habitos.org"))
  org-hide-emphasis-markers t)

(use-package toc-org)
(if (require 'toc-org nil t)
    (progn
      (add-hook 'org-mode-hook 'toc-org-mode))
  (warn "toc-org not found"))

(straight-use-package
 '(org-cliplink :type git :host github :repo "rexim/org-cliplink"))
(global-set-key (kbd "C-x p i") 'org-cliplink)

(use-package org-modern)

(global-org-modern-mode)
(setq org-modern-star '("✢" "✿" "❁" "✾" "❀" "✤" "❖"))

(use-package org-fancy-priorities
  :ensure t
  :hook
  (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("⚠" "‼" "❗")))

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("s" . "src"))

(require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)
(setq org-todo-keywords
    '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
      (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

(setq org-refile-targets
      '(("Archive.org" :maxlevel . 1)
        ("Tasks.org" :maxlevel . 1)))
(advice-add 'org-refile :after 'org-save-all-org-buffers)

;; MAYBE ADD LATER CAPTURE TEMPLATES
  (setq org-agenda-custom-commands
   '(("d" "Tablero"
     ((agenda "" ((org-deadline-warning-days 7)))
      (todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))
      (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

    ("n" "Next Tasks"
     ((todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))))

    ("W" "Work Tasks" tags-todo "+work-email")

    ;; Low-effort next actions
    ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
     ((org-agenda-overriding-header "Low Effort Tasks")
      (org-agenda-max-todos 20)
      (org-agenda-files org-agenda-files)))

    ("w" "Workflow Status"
     ((todo "WAIT"
            ((org-agenda-overriding-header "Waiting on External")
             (org-agenda-files org-agenda-files)))
      (todo "REVIEW"
            ((org-agenda-overriding-header "In Review")
             (org-agenda-files org-agenda-files)))
      (todo "PLAN"
            ((org-agenda-overriding-header "In Planning")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "BACKLOG"
            ((org-agenda-overriding-header "Project Backlog")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "READY"
            ((org-agenda-overriding-header "Ready for Work")
             (org-agenda-files org-agenda-files)))
      (todo "ACTIVE"
            ((org-agenda-overriding-header "Active Projects")
             (org-agenda-files org-agenda-files)))
      (todo "COMPLETED"
            ((org-agenda-overriding-header "Completed Projects")
             (org-agenda-files org-agenda-files)))
      (todo "CANC"
            ((org-agenda-overriding-header "Cancelled Projects")
             (org-agenda-files org-agenda-files)))))))

(setq org-capture-templates
    `(("t" "Tareas / Projectos")
      ("tt" "Tarea" entry (file+olp "~/Org/Haceres.org" "Transitorias")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

      ("j" "Entradas de Diario")
      ("jj" "Diario" entry
           (file+olp+datetree "~/Org/Diario.org")
           "\n* %<%I:%M %p> - Diario :journal:\n\n%?\n\n"
           ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
           :clock-in :clock-resume
           :empty-lines 1)
      ("jm" "Reuniones" entry
           (file+olp+datetree "~/Org/Diario.org")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)

      ("w" "Flujo Laboral")
      ("we" "Revisando Email" entry (file+olp+datetree "~/Org/Diario.org")
           "* Revisando Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

      ("m" "Captura de Metricas")
      ("mw" "Peso" table-line (file+headline "~/Org/Metricas.org" "Weight")
       ;; Ejemplo:
       "| %U | %^{Peso} | %^{Notas} |" :kill-buffer t)))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("✢" "✿" "❁" "✾" "❀" "✤" "❖")))

  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(with-eval-after-load 'org-faces
(set-face-attribute 'org-document-title nil :font "Iosevka Comfy Duo" :weight 'bold :height 1.3)
(dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Iosevka Comfy Duo" :weight 'regular :height (cdr face))))

(defun angl/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . angl/org-mode-visual-fill))

(defun efs/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name user-emacs-directory))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

(use-package term
  :commands term
  :config
  (setq explicit-shell-file-name "zsh") ;; Cambiar "zsh" a bash dependiento de tu maquina
  (setq term-prompt-regexp "%B%{$fg[red]%}[%{$fg[yellow]%}%n%{$fg[green]%}@%{$fg[blue]%}%M %{$fg[magenta]%}%~%{$fg[red]%}]%{$reset_color%}$%b"))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package vterm
  :commands vterm
  :config
  (setq term-prompt-regexp "%B%{$fg[red]%}[%{$fg[yellow]%}%n%{$fg[green]%}@%{$fg[blue]%}%M %{$fg[magenta]%}%~%{$fg[red]%}]%{$reset_color%}$%b")
  (setq vterm-shell "zsh")
  (setq vterm-max-scrollback 10000))

  (straight-use-package
   '(vterm-toggle :type git :host github :repo "jixiuf/vterm-toggle"))

(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.0)
  (corfu-quit-at-boundary 'separator)
  (corfu-echo-documentation 0.25)
  (corfu-preview-current 'insert)
  (corfu-preselect-first nil)
  :bind (:map corfu-map
              ;;("M-SCP"   . corfu-insert-separator)
              ("RET"     . nil)
              ("TAB"     . corfu-next)
              ([tab]     . corfu-next)
              ("S-TAB"   . corfu-previous)
              ([backtab] . corfu-previous)
              ("S-<return>" . corfu-insert))
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  (corfu-history-mode)
  :config
  (add-hook 'eshell-mode-hook
            (lambda () (setq-local corfu-quit-at-boundary t
                                   corfu-quit-no-match t
                                   corfu-auto nil)
              (corfu-mode))))

(setq corfu-popupinfo-delay (cons t 0.0))

;; Añade iconos para Corfu
(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package orderless
  :init
  (setq completion-styles '(orderless partial-completion basic)
        completion-category-defaults nil
        completion-category-overrides nil))

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
   :custom
   (lsp-completion-provider :none)
   :commands (lsp lsp-deferred)
   :init
   (defun angl/lsp-mode-setup-completion ()
     (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
           '(orderless)))
   (setq lsp-keymap-prefix "C-c l") ;; Puede ser "C-l" o "s-l"
   :hook
   (lsp-completion-mode . angl/lsp-mode-setup-completion)
   :config
   (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-ivy)

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package yasnippet
  :straight t
  :bind
  ("C-c y s" . yas-insert-snippet)
  ("C-c y v" . yas-visit-snippet-file)
  :config
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
  (yas-global-mode 1))

(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq gc-cons-threshold 100000000)

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package lsp-pyright
  :straight t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred))))

(use-package eglot)
(require 'eglot)
(add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)

(angl/leader-keys
  ;; Acciones en Org
  "o" '(:ignore t :which-key "Acciones en Org")
  "oA" '(org-agenda :which-key "Abrir Agenda")
  "ot" '(counsel-org-tag :which-key "Añadir Etiquetas")
  "oc" '(org-capture :which-key "Notas Rapidas")
  "oe" '(org-export-dispatch :which-key "Exportar")
  ;; Herramientas de Escritura     
  "w" '(:ignore t :which-key "Herramientas de Escritura")
  "wr" '(writeroom-mode :which-key "Alternar Modo de Escritura")
  ;; Elementos Visuales
  "v" '(:ignore t :which-key "Elementos Visuales")
  "vt" '(treemacs :which-key "Treemacs")
  "vs" '(lsp-treemacs-symbols :which-key "LSP Treemacs")
  ;; Terminales
  "t" '(:ignore t :which-key "Terminales")
  ; TODO tengo que arreglar vterm-toggle-cd, no funciona si la termina se deja abierta
  "to" '(vterm-toggle-cd :which-key "Abrir vterm"))
