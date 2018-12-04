;;; Package management:
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

(eval-when-compile
  (require 'use-package))
(require 'bind-key)

;;; Global:
;; Local settings:
(load "~/.emacs.d/lisp/local.el")

;; Startup:
(setq inhibit-startup-screen "t"
      initial-frame-alist (quote ((fullscreen . maximized)))
      ns-use-native-fullscreen nil
      mode-require-final-newline nil
      split-width-threshold 200
      exec-path (append exec-path '("/usr/local/bin")))
(display-time-mode "t")
(scroll-bar-mode 0)
(tool-bar-mode 0)

;; Deletion:
(global-set-key (read-kbd-macro "<M-DEL>")
                (lambda (arg)
                  (interactive "p")
                  (delete-region (point) (progn (forward-word (- arg)) (point)))))
(delete-selection-mode)

;; Transparency:
(set-frame-parameter (selected-frame) 'alpha '(85 . 85))
(add-to-list   'default-frame-alist '(alpha . (85 . 85)))

;; Fancy titlebar for MacOS:
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq frame-title-format nil)

;; Swiper:
(use-package swiper
  :bind
  ("C-s" . swiper))

;; Magit:
(use-package magit
  :init
  (setq magit-bury-buffer-function 'magit-mode-quit-window
        magit-repository-directories '(("~/.emacs.d/" . 0)
                                       ("~/.emacs.d/lib/" . 1)
                                       ("~/Sites/platform" . 0)))
  :bind
  ("C-x g" . magit-status))

(use-package magithub
  :after
  magit
  :ensure
  t
  :config
  (magithub-feature-autoinject t))

;; Purpose:
(use-package window-purpose
  :after
  magit
  :init
  (setq purpose-user-mode-purposes (quote ((web-mode        . html)
                                           (handlebars-mode . html)
                                           (js2-mode        . js)
                                           (css-mode        . css)
                                           (org-mode        . org)
                                           (php-mode        . php)
                                           (scala-mode      . scala)
                                           (magit-mode      . magit)))
        purpose-user-name-purposes (quote (("*Vagrant*" . vagrant)
                                           ("*gulp*"    . vagrant)
                                           ("*logs*"    . vagrant)
                                           ("*util*"    . vagrant)
                                           ("*test*"    . vagrant)
                                           ("*Group*"   . gnus))))
  :config
  (purpose-mode)
  (purpose-compile-user-configuration)
  (purpose-x-magit-single-on))

;; Vagrant:
(use-package vagrant
  :bind
  (("C-c C-v b" . vagrant-ssh) ; b for bash
   ("C-c C-v c" . vagrant-catalog-sync)
   ("C-c C-v e" . vagrant-edit-nginx-conf)
   ("C-c C-v g" . vagrant-gulp-logs)
   ("C-c C-v h" . vagrant-halt)
   ("C-c C-v l" . vagrant-error-logs)
   ("C-c C-v n" . vagrant-restart-nginx)
   ("C-c C-v p" . vagrant-provision)
   ("C-c C-v r" . vagrant-reload)
   ("C-c C-v s" . vagrant-restart-services)
   ("C-c C-v t" . vagrant-run-tests)
   ("C-c C-v u" . vagrant-up-provision))
  :config
  (defun vagrant-ssh-command (command buffer &optional persistent)
    (if (and persistent (get-buffer buffer))
        (switch-to-buffer-other-window buffer)
      (let ((old-directory default-directory))
        (cd "~/Sites/platform")
        (async-shell-command (format "vagrant ssh -c '%s'" command) buffer)
        (cd old-directory))))

  (defun vagrant-catalog-sync ()
    (interactive)
    (vagrant-ssh-command "sudo /vagrant/scripts/provision/vagrant.sh sync" "*util*"))

  (defun vagrant-gulp-logs ()
    (interactive)
    (vagrant-ssh-command "sudo /vagrant/scripts/provision/vagrant.sh gulp" "*gulp*" 1))

  (defun vagrant-error-logs ()
    (interactive)
    (vagrant-ssh-command "sudo /vagrant/scripts/provision/vagrant.sh logs" "*logs*" 1))

  (defun vagrant-restart-nginx ()
    (interactive)
    (vagrant-ssh-command "sudo service nginx restart" "*util*"))

  (defun vagrant-restart-services ()
    (interactive)
    (vagrant-ssh-command "sudo /vagrant/scripts/provision/vagrant.sh restart" "*util*"))

  (defun vagrant-run-tests ()
    (interactive)
    (vagrant-ssh-command "sudo /vagrant/scripts/provision/vagrant.sh test" "*test*"))

  (defun vagrant-up-provision ()
    (interactive)
    (let ((old-directory default-directory))
      (cd "~/Sites/platform")
      (async-shell-command "vagrant up --provision" "*Vagrant*")
      (cd old-directory)))
  )

;; Dumb Jump:
(use-package dumb-jump
  :init
  (setq dump-jump-force-searcher 'rg)
  :config
  (dumb-jump-mode))

;; Powerline:
(use-package powerline
  :config
  (powerline-default-theme))

(use-package airline-themes)

;;; Major modes:
;; JavaScript:
(use-package js2-mode
  :bind
  (:map js2-mode-map
        ("M-." . nil))
  :config
  (defun javascript-hook ()
    (setq js2-strict-missing-semi-warning nil
          js2-strict-trailing-comma-warning nil)
    (smartparens-mode)
    (flycheck-mode)
    (js2-refactor-mode)
    (js2r-add-keybindings-with-prefix "C-c C-m")
    (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))
  :hook
  (js2-mode . javascript-hook)
  :mode
  "\\.js\\'")

;; PHP:
(use-package php-mode
  :config
  (defun php-hook ()
    (smartparens-mode)
    (flycheck-mode)
    (php-enable-psr2-coding-style))
  :hook
  (php-mode . php-hook))

;; Web:
(use-package web-mode
  :init
  (setq web-mode-enable-auto-pairing nil
        web-mode-script-padding 0
        web-mode-engines-alist '(("php" . "\\.phtml\\'")))
  :mode
  "\\.html\\'" "\\.phtml\\'")

;; Elisp:
(use-package lisp-mode
  :config
  (defun emacs-lisp-hook ()
    (make-local-variable 'outline-regexp)
    (make-local-variable 'outline-heading-end-regexp)
    (setq outline-regexp "^;;"
          outline-heading-end-regexp ":\n")
    (outline-minor-mode 1))
  :hook
  (emacs-lisp-mode . emacs-lisp-hook)
  :ensure
  nil)

;; Ibuffer:
(use-package ibuffer
  :config
  (defun ibuffer-hook ()
    (ibuffer-auto-mode 1)
    (ibuffer-switch-to-saved-filter-groups "default"))
  :commands
  (ibuffer)
  :bind
  ("C-x C-b" . ibuffer)
  :init
  (setq ibuffer-show-empty-filter-groups nil
        ibuffer-expert t
        ibuffer-saved-filter-groups
        (quote (("default"
                 ("web" (or (mode . css-mode)
                            (mode . handlebars-mode)
                            (mode . js2-mode)
                            (mode . php-mode)
                            (mode . scss-mode)
                            (mode . web-mode)))
                 ("org" (mode . org-mode))
                 ("magit" (name . "\\*magit"))
                 ("shell" (mode . shell-mode))
                 ("emacs" (or
                           (name . "^\\*scratch\\*$")
                           (name . "^\\*Messages\\*$")
                           (name . "^\\*Help\\*$")
                           (name . "^\\*Completions\\*$")
                           (name . "^init.el$")))
                 ("gnus" (or
                          (mode . message-mode)
                          (mode . bbdb-mode)
                          (mode . mail-mode)
                          (mode . gnus-group-mode)
                          (mode . gnus-summary-mode)
                          (mode . gnus-article-mode)
                          (name . "^\\.bbdb$")
                          (name . "^\\.newsrc-dribble")))
                 ("dirs" (mode . dired-mode))
                 ("scala" (mode . scala-mode))
                 ("rg"   (mode . arg-mode))))))
  :hook
  (ibuffer-mode . ibuffer-hook))

;; Org:
(use-package org
  :init
  (setq org-startup-folded "showeverything")
  :config
  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)   ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
  :hook
  (org-after-todo-statistics-hook . org-summary-todo))

;; Scala:
(use-package ensime
  :init
  (setq ensime-startup-notification nil
        ensime-sbt-command "/usr/local/sbt/bin/sbt"
        sbt:program-name "/usr/local/sbt/bin/sbt"))

;; ripgrep:
(use-package rg
  :config
  (rg-enable-default-bindings))

;;; Minor modes:
;; Disable abbrev mode:
(setq abbrev-mode nil)

;; Smartparens:
(use-package smartparens
  :config
  (require 'smartparens-config)
  :bind
  (("C-<right>" . sp-forward-slurp-sexp)
   ("M-<right>" . sp-forward-barf-sexp)
   ("C-<left>"  . sp-backward-slurp-sexp)
   ("M-<left>"  . sp-backward-barf-sexp)))

;; Logs:
(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-tail-mode))

;;; Gnus:
(use-package gnus
  :init
  (defun agnus() (interactive) (make-thread #'gnus))
  :config
  (setq gnus-select-method '(nnnil "")
        gnus-save-newsrc-file nil
        gnus-home-directory "~/.emacs.d/"
        message-directory  "~/.emacs.d/Mail/"
        nnfolder-directory  "~/.emacs.d/Mail/"
        gnus-message-archive-method '(nnfolder "archive"
                                               (nnfolder-directory "~/.emacs.d/Mail/archive")
                                               (nnfolder-active-file "~/.emacs.d/Mail/archive/active")
                                               (nnfolder-get-new-mail nil)
                                               (nnfolder-inhibit-expiry t))
        gnus-directory "~/.emacs.d/News/"
        gnus-use-full-window nil
        gnus-always-read-dribble-file t)
  ;; Reddit:
  (require 'nnreddit "~/.emacs.d/lisp/nnreddit.el")
  (add-to-list 'gnus-secondary-select-methods
               '(nnreddit ""))
  ;; Atom feeds:
  (require 'mm-url)
  (defadvice mm-url-insert (after DE-convert-atom-to-rss () )
    "Converts atom to RSS by calling xsltproc."
    (when (re-search-forward "xmlns=\"http://www.w3.org/.*/Atom\""
                             nil t)
      (goto-char (point-min))
      (message "Converting Atom to RSS... ")
      (call-process-region (point-min) (point-max)
                           "xsltproc"
                           t t nil
                           (expand-file-name "~/.emacs.d/atom2rss.xsl") "-")
      (goto-char (point-min))
      (message "Converting Atom to RSS... done")))
  (ad-activate 'mm-url-insert))

;;; Indentation:
(setq my-indent 4
      js2-basic-offset my-indent
      c-basic-offset my-indent
      css-indent-offset my-indent
      handlerbars-basic-offset my-indent
      c-default-style "linux")
(setq-default indent-tabs-mode nil)

;;; Shell:
(add-to-list 'same-window-buffer-names "*shell*")
(setenv "PATH" (concat "~/bin:/usr/local/bin:/usr/local/sbt/bin:" (getenv "PATH")))

;;; Music:
;; Pianobar:
(use-package pianobar
  :init
  (setq pianobar-command "/usr/local/bin/pianobar"
        pianobar-username "baturkey@gmail.com"
        pianobar-station 9)
  :bind
  (("<f13>" . pianobar-start-or-play-or-pause)
   ("<f14>" . pianobar-next-song)
   ("<f15>" . pianobar-ban-current-song))
  :config
  (defun pianobar-start-or-play-or-pause ()
    (interactive)
    (if (comint-check-proc "*pianobar*")
        (pianobar-play-or-pause)
      (pianobar))))

;; EMMS:
(use-package emms
  :init
  (setq emms-info-functions '(emms-info-libtag)
        emms-player-list    `(emms-player-afplay))
  :bind
  (("<f16>" . emms-previous)
   ("<f17>" . emms-pause)
   ("<f18>" . emms-next)
   ("<f19>" . emms-browser))
  :config
  (require 'emms-info-libtag)
  (require 'emms-player-simple)
  (emms-all)
  (define-emms-simple-player afplay '(file)
    (regexp-opt '(".mp3" ".m4a" ".aac"))
    "afplay"))


(use-package mastodon
  :config
  (setq mastodon-instance-url "https://flumph.masto.host")

  (bind-keys
   :map mastodon-mode-map
   ("#" . mastodon-tl--get-tag-timeline)
   ("A" . mastodon-profile--get-toot-author)
   ("F" . mastodon-tl--get-federated-timeline)
   ("H" . mastodon-tl--get-home-timeline)
   ("L" . mastodon-tl--get-local-timeline)
   ("M-n" . mastodon-tl--next-tab-item)
   ("M-p" . mastodon-tl--previous-tab-item)
   ("N" . mastodon-notifications--get)
   ("P" . mastodon-profile--show-user)
   ("Q" . kill-buffer-and-window)
   ("T" . mastodon-tl--thread)
   ("b" . mastodon-toot--toggle-boost)
   ("c" . mastodon-tl--toggle-spoiler-text-in-toot)
   ("f" . mastodon-toot--toggle-favourite)
   ("g" . mastodon-tl--update)
   ("n" . mastodon-tl--goto-next-toot)
   ("p" . mastodon-tl--goto-prev-toot)
   ("r" . mastodon-toot--reply)
   ("t" . mastodon-toot)
   ("y" . bury-buffer)               ;Only bury
   ("z" . quit-window)))             ;Quit + bury

;;; Weather:
(use-package wttrin
  :init
  (setq wttrin-default-cities          '("New York" "Chicago")
        wttrin-default-accept-language '("Accept-Language" . "en-US")))

;;; Custom:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(comint-process-echoes t)
 '(custom-enabled-themes (quote (tango-dark airline-doom-one)))
 '(custom-safe-themes
   (quote
    ("a94f1a015878c5f00afab321e4fef124b2fc3b823c8ddd89d360d710fc2bddfc" "0cd56f8cd78d12fc6ead32915e1c4963ba2039890700458c13e12038ec40f6f5" "3eb93cd9a0da0f3e86b5d932ac0e3b5f0f50de7a0b805d4eb1f67782e9eb67a4" "251348dcb797a6ea63bbfe3be4951728e085ac08eee83def071e4d2e3211acc3" "01e067188b0b53325fc0a1c6e06643d7e52bc16b6653de2926a480861ad5aa78" "721bb3cb432bb6be7c58be27d583814e9c56806c06b4077797074b009f322509" "946e871c780b159c4bb9f580537e5d2f7dba1411143194447604ecbaf01bd90c" "8aca557e9a17174d8f847fb02870cb2bb67f3b6e808e46c0e54a44e3e18e1020" "1c082c9b84449e54af757bcae23617d11f563fc9f33a832a8a2813c4d7dfb652" "3e6b5d23ee79d0613584bc3279eae87b1d4cf4105732db60902b391c3d94b978" "d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "7e78a1030293619094ea6ae80a7579a562068087080e01c2b8b503b27900165c" "a3fa4abaf08cc169b61dea8f6df1bbe4123ec1d2afeb01c17e11fdc31fc66379" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(custom-theme-allow-multiple-selections t)
 '(eshell-modules-list
   (quote
    (eshell-alias eshell-banner eshell-basic eshell-cmpl eshell-dirs eshell-glob eshell-hist eshell-ls eshell-pred eshell-prompt eshell-rebind eshell-script eshell-term eshell-unix)))
 '(grep-command "grep --exclude \"*.min.*\" -nHR -e   .")
 '(logview-additional-timestamp-formats
   (quote
    (("Magento"
      (java-pattern . "YYYY-MM-dd'T'HH:mm:ss+00:00")))))
 '(package-selected-packages
   (quote
    (airline-themes powerline org-jira amx emojify mastodon git-gutter+ sql-indent magithub rg hackernews csv-mode swiper wttrin xref-js2 edbi-database-url edbi js2-refactor popup-imenu ensime dumb-jump vagrant-tramp restclient logview window-purpose use-package handlebars-mode emms smartparens flycheck org pianobar vagrant babel markdown-mode gnugo json-mode python-mode magit php-mode web-mode)))
 '(save-interprogram-paste-before-kill t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'emms-browser-delete-files 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
