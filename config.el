;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(after! doom-modeline
  (setq doom-modeline-display-default-persp-name t
        doom-modeline-persp-name t ;; TODO make it easier to see
        doom-modeline-mu4e t ;; TODO where is this exactly?
        doom-modeline-buffer-encoding nil))

;; default to english.
(setq ispell-dictionary "en")

;; default is 1 second which is a bit slow
(setq which-key-idle-delay 0.3)
;; (which-key-mode 1)

;; helps avoid file sync issues
(global-auto-revert-mode 1)


;; this will draw a vertical line to indicate line length
(global-display-fill-column-indicator-mode 0)
(setq-default fill-column 100)

;; help entries are often more long form. put them in a proper window
(set-popup-rule! "^\\*\\([Hh]elp\\|Apropos\\)" :ignore t)

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Shuwen Sun"
      user-mail-address "sun.shuw@northeastern.edu")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;; (setq doom-font (font-spec :family "Fira Code" :size 18 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 19))
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 18 :weight 'semi-light)
     doom-variable-pitch-font (font-spec :family "JetBrainsMono Nerd Font" :size 19))

;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one)
(setq doom-theme 'doom-material)

(setq-default line-spacing 0.2)

(use-package! mixed-pitch
  :hook ((org-mode . mixed-pitch-mode)
         (markdown-mode . mixed-pitch-mode)))
  ;; :config
  ;; (setq mixed-pitch-set-heigth t)
  ;; (set-face-attribute 'variable-pitch nil :height 1.3))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; use visible buffer, not just the current line
(setq evil-snipe-scope 'visible)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org/")

(load! "colors")
(load! "keybindings")

(global-visual-line-mode) ;; Always wrap long lines
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow)) ;; Show arrows on wrapped lines

;; Switch to the new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)

(setq confirm-kill-emacs nil)

;; Backups. Make a bunch
(setq make-backup-files t
      version-control t
      kept-new-versions 10
      kept-old-versions 2
      delete-old-versions t
      backup-by-copying t  ;; Copy all files, don't rename them.
      vc-make-backup-files t
      backup-directory-alist '((".*" . "~/.emacs.d.backup")))


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;

;; decrease the timeout before jumping around the buffer
(setq avy-timeout-seconds 0.3)

;; If tooltips turned on, make tips appear promptly
(setq tooltip-delay 0.1)  ; default is 0.7 second)

;; this is a hack that allows us to use lookups to other windows
;; https://github.com/hlissner/doom-emacs/issues/3397
(dolist (fn '(definition references))
  (fset (intern (format "+lookup/%s-other-window" fn))
        (lambda (identifier &optional arg)
          "TODO"
          (interactive (list (doom-thing-at-point-or-region)
                             current-prefix-arg))
          (let ((pt (point)))
            (switch-to-buffer-other-window (current-buffer))
            (goto-char pt)
            (funcall (intern (format "+lookup/%s" fn)) identifier arg)))))

;; (after! tramp
;;   (add-to-list 'tramp-methods
;;                '("yadm"
;;                  (tramp-login-program "yadm")
;;                  (tramp-login-args (("enter")))
;;                  (tramp-login-env (("SHELL") ("/bin/sh")))
;;                  (tramp-remote-shell "/bin/sh")
;;                  (tramp-remote-shell-args ("-c")))))
;;
;; (load! "colors")
;; (load! "keybindings")


;; (after! elfeed
;;   (setq elfeed-search-filter "@2-month-ago +unread"))

(use-package! dired
  :hook (dired-mode . dired-hide-details-mode)
  :custom
  (setq dired-dwim-target t ;; guess default target directory
        dired-recursive-copies 'always
        dired-recursive-deletes 'always
        dired-kill-when-opening-new-dired-buffer t))

(use-package! evil-collection)

;; (use-package! mu4e
;;   :config
;;   (require 'mu4e-headers)
;;   (defun bergheim/mail-search (query)
;;     "Perform a mu4e query"
;;     (interactive)
;;     (=mu4e)
;;     (mu4e-search-bookmark query))
;;
;;   (defun bergheim/mu4e-email-today(&optional lookback)
;;     "Opens the inbox with unread and by default shows todays email
;;
;; If LOOKBACK is specified, use that instead of 1d.
;; If \\[universal-argument] if called before this, show a week back."
;;     (interactive)
;;     (require 'mu4e)
;;     (let ((mu4e-search-include-related t)
;;           (mu4e-search-threads t)
;;           (mu4e-search-sort-field :date)
;;           (mu4e-search-sort-direction :ascending))
;;
;;       ;; ask if you want to apply any changes made before leaving
;;       (mu4e-mark-handle-when-leaving)
;;
;;       (unless lookback
;;         (setq lookback "2m"))
;;       (if current-prefix-arg
;;           (setq lookback "1y"))
;;
;;       (=mu4e t)
;;       ;; Add the hook temporarily
;;       (add-hook 'mu4e-headers-found-hook #'bergheim/mu4e--headers-goto-bottom)
;;       (mu4e-search (concat "maildir:/Inbox/ AND date:" lookback "..now"))))
;;
;;   (defun bergheim/mu4e-email-sent()
;;     (interactive)
;;     (=mu4e)
;;     (mu4e-search-bookmark "maildir:/Sent/")))
;;
;; (after! mu4e (load! "+mu4e"))
(after! org (load! "+org"))

(load! "completion")

(use-package! orgit
  ;; Automatically copy orgit link to last commit after commit
  :hook (git-commit-post-finish . orgit-store-after-commit)
  :config
  (defun orgit-store-after-commit ()
    "Store orgit-link for latest commit after commit message editor is finished."
    (let* ((repo (abbreviate-file-name default-directory))
           (rev (magit-git-string "rev-parse" "HEAD"))
           (link (format "orgit-rev:%s::%s" repo rev))
           (summary (substring-no-properties (magit-format-rev-summary rev)))
           (desc (format "%s (%s)" summary repo)))
      (push (list link desc) org-stored-links))))

(use-package! affe
  :config
    ;; (setq affe-find-command "fd -HI -t f")
    (setq affe-find-command "rg --color=never --hidden --files"))

;; (defun bergheim/toggle-maximize ()
;;   (interactive)
;;   (if (get 'bergheim/toggle-maximize 'enabled)
;;       (progn
;;         (put 'bergheim/toggle-maximize 'enabled nil)
;;         (winner-undo))
;;       (progn
;;         (put 'bergheim/toggle-maximize 'enabled t)
;;         (doom/window-maximize-buffer))))

(use-package! auto-dim-other-buffers
  :hook (after-init . auto-dim-other-buffers-mode))

;; (use-package! calendar-norway
;;   :config
;;   (setq calendar-holidays
;;         (append
;;          ;; Include days where you don't have to work:
;;          calendar-norway-raude-dagar
;;          ;; Include other days that people celebrate:
;;          calendar-norway-andre-merkedagar
;;          ;; Include daylight savings time:
;;          calendar-norway-dst
;;          ;; And then you can add some non-Norwegian holidays etc. if you like:
;;          '((holiday-fixed 3 17 "St. Patrick's Day")
;;            (holiday-fixed 10 31 "Halloween")
;;            (holiday-float 11 4 4 "Thanksgiving")
;;            (solar-equinoxes-solstices)))))

(use-package! evil-matchit
  :config
  (global-evil-matchit-mode 1))

;;undotree
(after! undo-tree
  (setq undo-tree-auto-save-history t)
  (map! :leader "u" #'undo-tree-visualize))

;;treesitter

(use-package! tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; treemacs
(after! treemacs
  (setq treemacs-position 'right
            treemacs-show-hidden-files               nil
        treemacs-git-mode 'deferred))

;; (use-package doom-modeline
;;   :init (doom-modeline-mode 1)
;;   :custom
;;   (doom-modeline-height 25)     ;; Sets modeline height
;;   (doom-modeline-bar-width 5)   ;; Sets right bar width
;;   (doom-modeline-persp-name t)  ;; Adds perspective name to modeline
;;   (doom-modeline-persp-icon t)) ;; Adds folder icon next to persp name

;; vterm
;; (after! vterm
;;   (set-popup-rule! "*doom:vterm-popup:main" :size 0.4 :select t :quit nil :side 'right)
;;   )

;; yasnippets
(setq yas-snippet-dirs '("~/.config/doom/snippets"))

(setq ispell-program-name "aspell"
      ispell-dictionary "english")

;; https://emacs.stackexchange.com/questions/2871/keeping-my-org-files-in-sync-across-multiple-computers
(add-hook 'auto-save-hook 'org-save-all-org-buffers)
