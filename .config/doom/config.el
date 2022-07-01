;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;;(setq doom-theme 'doom-one)
;;(setq doom-theme 'afternoon)
(setq doom-theme 'klere)
(add-to-list 'default-frame-alist '(background-mode . dark))
;;(setq doom-theme ')

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
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
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; start of my config

;; screen size
;;(add-hook 'window-setup-hook #'toggle-frame-maximized)
(setq initial-frame-alist '((top . 1) (right . 1) (width . 150) (height . 150)))

;; font
(setq doom-font (font-spec :family "TerminessTTF Nerd Font Mono" :size 16))

;; dashboard
;;(setq fancy-splash-image "/Users/kylegortych/Downloads/doom-emacs-bw-light.svg")

(defun skull ()
  (let* ((banner '("   .o oOOOOOOOo                                            OOOo    "
                   "   Ob.OOOOOOOo  OOOo.      oOOo.                      .adOOOOOOO   "
                   "   OboO000000000000.OOo. .oOOOOOo.    OOOo.oOOOOOo..0000000000OO   "
                   "   OOP.oOOOOOOOOOOO iPOOOOOOOOOOOo.   `iOOOOOOOOOP,OOOOOOOOOOOB'   "
                   "   `O'OOOO'     `OOOOo'OOOOOOOOOOO` .adOOOOOOOOO'oOOO'    `OOOOo   "
                   "   .OOOO'            `OOOOOOOOOOOOOOOOOOOOOOOOOO'            `OO   "
                   "   OOOOO                 'iOOOOOOOOOOOOOOOOi`                oOO   "
                   "  oOOOOOba.                .adOOOOOOOOOOba               .adOOOOo. "
                   " oOOOOOOOOOOOOOba.    .adOOOOOOOOOO@^OOOOOOOba.     .adOOOOOOOOOOOO"
                   "OOOOOOOOOOOOOOOOO.OOOOOOOOOOOOOO'`  ''OOOOOOOOOOOOO.OOOOOOOOOOOOOO "
                   "'OOOO'       'YOoOOOOMOIONODOO'`  .   ''OOROAOPOEOOOoOY'     'OOO' "
                   "   Y           'OOOOOOOOOOOOOO: .oOOo. :OOOOOOOOOOO?'         :`   "
                   "   :            .oO%OOOOOOOOOOo.OOOOOO.oOOOOOOOOOOOO?         .    "
                   "   .            oOOPi%OOOOOOOOoOOOOOOO?oOOOOO?OOOOiOOo             "
                   "                '%o  OOOO'%OOOO%'%OOOOO'OOOOOO'OOO':               "
                   "                     `$i  `OOOO' `O'Y ' `OOOO'  o             .    "
                   "   .                  .     OP'          : o     .                 "
                   "                             :                                     "
                   "                             .                                     "
                   "                                                                   "))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat line (make-string (max 0 (- longest-line (length line))) 68)))
               "\n"))
     'face 'doom-dashboard-banner)))

(setq +doom-dashboard-ascii-banner-fn #'skull)

(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-loaded)
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-footer)

(add-hook! '+doom-dashboard-functions :append
  (insert "\n" (+doom-dashboard--center +doom-dashboard--width "config by Kyle Gortych")))
;; apply icons to dired?


;; disable quit comfirmation
(setq confirm-kill-emacs nil)

;; cursor shape
(unless (display-graphic-p)
        (require 'evil-terminal-cursor-changer)
        (evil-terminal-cursor-changer-activate) ; or (etcc-on)
        )

(setq evil-insert-state-cursor '(hbar "white")
      evil-normal-state-cursor '(box "white")
      evil-motion-state-cursor '(box "white")
      evil-visual-state-cursor '(box "white")
      evil-emacs-state-cursor '(box "white"))

;; note: org export html | dosn't include superstar | css file to change behavior

;; orgmode bullets
(setq org-superstar-headline-bullets-list '("➀" "➁" "➂" "➃" "➄"))

;; orgmode list symbol
(setq org-superstar-item-bullet-alist '((?+ . ?») (?- . ?») (?➤ . ?»)))

;; stripe-buffer
(add-hook 'dired-mode-hook 'turn-on-stripe-buffer-mode)
(add-hook 'dired-mode-hook 'stripe-listify-buffer)
(add-hook 'org-mode-hook 'turn-on-stripe-table-mode)

;; beacon
(beacon-mode 1)
;; (beacon-color "#00afff")
;; (beacon--colored-overlay )

;; lsp-sonarlint
(require 'lsp-sonarlint)

(require 'lsp-sonarlint-php)
(setq lsp-sonarlint-php-enabled t)

(require 'lsp-sonarlint-html)
(setq lsp-sonarlint-html-enabled t)

(require 'lsp-sonarlint-javascript)
(setq lsp-sonarlint-javascript-enabled t)

(require 'lsp-sonarlint-typescript)
(setq lsp-sonarlint-typescript-enabled t)
