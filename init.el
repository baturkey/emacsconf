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
(load "~/.emacs.d/lisp/local")

;; Startup:
(setq inhibit-startup-screen "t"
      initial-frame-alist (quote ((fullscreen . maximized)))
      ns-use-native-fullscreen nil
      mode-require-final-newline nil
      split-width-threshold 200
      exec-path (append exec-path '("/usr/local/ImageMagick-7.0.8/bin" "/usr/local/bin")))
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
        magit-display-buffer-function (lambda (buffer)
                                        (display-buffer
                                         buffer (if (and (derived-mode-p 'magit-mode)
                                                         (memq (with-current-buffer buffer major-mode)
                                                               '(magit-process-mode
                                                                 magit-revision-mode
                                                                 magit-diff-mode
                                                                 magit-stash-mode
                                                                 magit-status-mode)))
                                                    nil
                                                  '(display-buffer-same-window))))
        magit-repository-directories '(("~/.emacs.d/" . 0)
                                       ("~/.emacs.d/lib/" . 1)
                                       ("~/Sites/platform/" . 0)
                                       ("~/bin/eulerjs/" . 0)))
  :bind
  ("C-x g" . magit-status))

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
  (("C-c b" . vagrant-ssh) ; b for bash
   ("C-c c" . vagrant-catalog-sync)
   ("C-c e" . vagrant-edit-nginx-conf)
   ("C-c f" . vagrant-fetch-product-json)
   ("C-c g" . vagrant-gulp-logs)
   ("C-c h" . vagrant-halt)
   ("C-c l" . vagrant-error-logs)
   ("C-c n" . vagrant-restart-nginx)
   ("C-c p" . vagrant-provision)
   ("C-c r" . vagrant-reload)
   ("C-c s" . vagrant-restart-services)
   ("C-c t" . vagrant-run-tests)
   ("C-c u" . vagrant-up-provision))
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
    (async-shell-command "~/bin/stagescala.sh" "*util*"))

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

  (defun vagrant-fetch-product-json ()
    (interactive)
    (vagrant-ssh-command (format "curl --silent http://localhost:8000/products/%s | python -m json.tool" (read-string "Enter PID:")) "*test-json*"))
  )

;; Dumb Jump:
(use-package dumb-jump
  :config
  (setq dumb-jump-force-searcher 'rg))

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
  "\\.html\\'" "\\.phtml\\'" "\\.handlebars\\'")

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
(setenv "PATH" (concat "~/bin:/usr/local/ImageMagick-7.0.8/bin:/usr/local/bin:/usr/local/sbt/bin:" (getenv "PATH")))
(setenv "MAGICK_HOME" "/usr/local/ImageMagick-7.0.8")
(setenv "DYLD_LIBRARY_PATH" "/usr/local/ImageMagick-7.0.8/lib")

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

;;; Mail:
(use-package mu4e
  :config
  (setq mu4e-drafts-folder "/mu4edrafts"
        mu4e-get-mail-command "offlineimap"
        mu4e-update-interval 300))

;;; Mastodon:
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

(defun eshell-top ()
  (interactive)
  (eshell-command "top"))

(bind-key "C-c =" 'eshell-top)

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
    ("a94f1a015878c5f00afab321e4fef124b2fc3b823c8ddd89d360d710fc2bddfc" default)))
 '(custom-theme-allow-multiple-selections t)
 '(eshell-modules-list
   (quote
    (eshell-alias eshell-banner eshell-basic eshell-cmpl eshell-dirs eshell-glob eshell-hist eshell-ls eshell-pred eshell-prompt eshell-rebind eshell-script eshell-term eshell-unix)))
 '(logview-additional-timestamp-formats
   (quote
    (("Magento"
      (java-pattern . "YYYY-MM-dd'T'HH:mm:ss+00:00")))))
 '(package-selected-packages
   (quote
    (forge inf-mongo malyon airline-themes powerline emojify mastodon git-gutter+ sql-indent rg hackernews csv-mode swiper wttrin xref-js2 edbi-database-url edbi js2-refactor popup-imenu ensime dumb-jump vagrant-tramp restclient logview window-purpose use-package emms smartparens flycheck org pianobar vagrant babel markdown-mode gnugo json-mode python-mode magit php-mode web-mode)))
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
