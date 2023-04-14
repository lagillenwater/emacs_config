;; Mac  specific key mappings
(when (eq system-type 'darwin)
  (setq
   ns-command-modifier 'super
   ns-option-modifier 'meta
   ns-control-modifier 'control
   ns-function-modifier 'hyper))

  (global-set-key (kbd "s-c") 'kill-ring-save)
  (global-set-key (kbd "s-v") 'yank)
  (global-set-key (kbd "s-x") 'kill-region)
  (global-set-key (kbd "s-a") 'mark-whole-buffer)
  (global-set-key (kbd "s-z") 'undo)
  (global-set-key (kbd "s-y") 'undo-redo)
  (global-set-key (kbd "s-f") 'swiper)
  (global-set-key (kbd "s-g") 'keyboard-escape-quit)
  (global-set-key (kbd "s-p") 'mac-preview) ; requires mac-preview
  (global-set-key (kbd "s-w") 'kill-buffer)
  (global-set-key (kbd "s-m") 'iconify-frame)
  (global-set-key (kbd "s-q") 'save-buffers-kill-emacs)
  (global-set-key (kbd "s-.") 'keyboard-quit)
  (global-set-key (kbd "s-l") 'goto-line)
  ;;(global-set-key (kbd "s-<up>")    'beginning-of-buffer)
  ;;(global-set-key (kbd "s-<down>")  'end-of-buffer)
  ;;(global-set-key (kbd "s-<left>")  'beginning-of-line)
  ;;d(global-set-key (kbd "s-<right>") 'end-of-line)
 ;; (global-set-key [(meta down)]     'forward-paragraph)
 ;; (global-set-key [(meta up)]       'backward-paragraph)


;; start message
(setq inhibit-startup-message t)

(scroll-bar-mode -1) ; Disable visible scrollbar
(tool-bar-mode -1)   ; Disable the toobar
(tooltip-mode -1)    ; Diable tooltips
(set-fringe-mode 10) ; 
(menu-bar-mode -1)

; visible bell instead of audible bell
(setq visible-bell t)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

  ;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; ivy for completion
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
  (ivy-mode 1));; ivy
(use-package ivy
  :diminish t
  :config
  (ivy-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))


(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))


(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Projects/CodeOneDrive - The University of Colorado Denver/Projects/Code")
    (setq projectile-project-search-path '("~/OneDrive - The University of Colorado Denver/Projects/Projects/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))


;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
(use-package forge) 
(setq auth-sources '("~/.authinfo"))



(use-package ess)
(use-package poly-R)
;; associate the new polymode to Rmd files:
(add-to-list 'auto-mode-alist
             '("\\.[rR]md\\'" . poly-gfm+r-mode))




;;toggle between light and dark theme
(use-package heaven-and-hell
  :ensure t
  :init`
  (setq heaven-and-hell-themes
        '((light . tsdh-light)
          (dark . tsdh-dark)))
  (setq heaven-and-hell-theme-type 'dark) ;; Omit to use light by default
   ;; Themes can be the list: (dark . (tsdh-dark wombat))
  ;; Optionall, load themes without asking for confirmation.
  (setq heaven-and-hell-load-theme-no-confirm t)
  :hook (after-init . heaven-and-hell-init-hook)
  :bind (("C-c <f6>" . heaven-and-hell-load-default-theme)
         ("<f6>" . heaven-and-hell-toggle-theme)))





;; org mode
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-agenda-files (list "~/org/school.org" 
                             "~/org/home.org"
			     "~/org/CPBS.org"))

(add-hook 'org-mode-hook #'(lambda ()

                             ;; make the lines in the buffer wrap around the edges of the screen.
                             
                             ;; to press C-c q  or fill-paragraph ever again!
                             (visual-line-mode)
                             (org-indent-mode)))


;; helm-bibtex related stuff
(use-package helm-bibtex
    :custom
    ;; In the lines below I point helm-bibtex to my default library file.
    (bibtex-completion-bibliography '("/Users/lucas/OneDrive - The University of Colorado Denver/Bibliography/bibliography.bib"))
    (reftex-default-bibliography '("/Users/lucas/OneDrive - The University of Colorado Denver/Bibliography/bibliography.bib"))
    ;; The line below tells helm-bibtex to find the path to the pdf
    ;; in the "file" field in the .bib file.
    (bibtex-completion-pdf-field "file"))
 
 


;; (use-package org-ref
;;   :ensure t
;;   :init
;;   :config
;;   (setq reftex-default-bibliography '("/Users/lucas/OneDrive - The University of Colorado Denver/Bibliography/bibliography.bib")
;; 	org-ref-default-bibliography (list "/Users/lucas/OneDrive - The University of Colorado Denver/Bibliography/bibliography.bib"))
;;   )

 



(use-package org-roam
  :ensure t
  :init
  :custom
  (org-roam-directory "/Users/lucas/OneDrive - The University of Colorado Denver/Notes/")
  (org-roam-capture-templates'(("d" "default" plain "%?"
   :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
   :unnarrowed t)))
  (org-roam-completion-everywhere t)
  (org-roam-dailies-capture-templates
    '(("d" "default" entry "* %<%I:%M %p>: %?"
       :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (org-roam-db-autosync-mode))

(global-auto-revert-mode 1)


(defun my/org-roam-copy-todo-to-today ()
  (interactive)
  (let ((org-refile-keep t) ;; Set this to nil to delete the original!
        (org-roam-dailies-capture-templates
          '(("t" "tasks" entry "%?"
             :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n" ("Tasks")))))
        (org-after-refile-insert-hook #'save-buffer)
        today-file
        pos)
    (save-window-excursion
      (org-roam-dailies--capture (current-time) t)
      (setq today-file (buffer-file-name))
      (setq pos (point)))

    ;; Only refile if the target file is different than the current file
    (unless (equal (file-truename today-file)
                   (file-truename (buffer-file-name)))
      (org-refile nil nil (list "Tasks" today-file nil pos)))))










;; flyspell
    (defun flyspell-on-for-buffer-type ()
      "Enable Flyspell appropriately for the major mode of the current buffer.  Uses `flyspell-prog-mode' for modes derived from `prog-mode', so only strings and comments get checked.  All other buffers get `flyspell-mode' to check all text.  If flyspell is already enabled, does nothing."
      (interactive)
      (if (not (symbol-value flyspell-mode)) ; if not already on
	(progn
	  (if (derived-mode-p 'prog-mode)
	    (progn
	      (message "Flyspell on (code)")
	      (flyspell-prog-mode))
	    ;; else
	    (progn
	      (message "Flyspell on (text)")
	      (flyspell-mode 1)))
	  ;; I tried putting (flyspell-buffer) here but it didn't seem to work
	  )))
    
    (defun flyspell-toggle ()
      "Turn Flyspell on if it is off, or off if it is on.  When turning on, it uses `flyspell-on-for-buffer-type' so code-vs-text is handled appropriately."
      (interactive)
      (if (symbol-value flyspell-mode)
	  (progn ; flyspell is on, turn it off
	    (message "Flyspell off")
	    (flyspell-mode -1))
	  ; else - flyspell is off, turn it on
	(flyspell-on-for-buffer-type)))
(global-set-key (kbd "C-c f") 'flyspell-toggle )

(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)))



;; word counting
(add-to-list 'load-path "~/.emacs.d/org-tracktable")
(load"org-tracktable.el")
(setq org-tracktable-daily-goal 1000)


;; to try and fix r behavior of going grey sometimes
(use-package xterm-color

  :init
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions))

  (add-hook 'inferior-ess-mode-hook
            (lambda () (add-hook 'comint-preoutput-filter-functions #'xterm-color-filter nil t)))

  :config
  (setq xterm-color-use-bold t))


;; backup files in a single folder 
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))

;; Adding clocking functions to track time
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)




;;eaf
(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")
(require 'eaf)
(require 'eaf-browser)



(use-package anki-editor)
(use-package anki-connect)



;;; company
(require 'company)
(setq tab-always-indent 'complete)

(setq company-idle-delay 0.5
      company-show-numbers t
      company-minimum-prefix-length 2
      company-tooltip-flip-when-above t)

(global-set-key (kbd "C-M-/") #'company-complete)
(global-company-mode)

;;; ESS
(defun my-ess-hook ()
  ;; ensure company-R-library is in ESS backends
  (make-local-variable 'company-backends)
  (cl-delete-if (lambda (x) (and (eq (car-safe x) 'company-R-args))) company-backends)
  (push (list 'company-R-args 'company-R-objects 'company-R-library :separate)
        company-backends))

(add-hook 'ess-mode-hook 'my-ess-hook)

(with-eval-after-load 'ess
  (setq ess-use-company t))

;; winmove default key bindings for quick movements around windows
(windmove-default-keybindings)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(wombat))
 '(ein:jupyter-default-server-command "/opt/homebrew/Caskroom/miniconda/base/bin/jupyter")
 '(package-selected-packages
   '(helm-bibtex pdf-tools org-pdftools jupyter ein ox-hugo anki-connect anki-editor xterm-color yasnippet which-key virtualenv use-package tern-auto-complete rainbow-delimiters pyvenv python-mode poly-R org-plus-contrib org-noter lsp-ui lsp-ivy jedi ivy-rich helpful heaven-and-hell frame-local forge flycheck eval-in-repl ess doom-themes doom-modeline deft dap-mode counsel-projectile company-jedi command-log-mode citar auto-complete-sage)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
