;;; packages.el --- C/C++ Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq c-c++-packages
  '(
    cc-mode
    disaster
    clang-format
    cmake-mode
    company
    company-c-headers
    irony
    company-irony
    flycheck
    gdb-mi
    helm-cscope
    helm-gtags
    semantic
    stickyfunc-enhance
    xcscope
    ))

(unless (version< emacs-version "24.4")
  (add-to-list 'c-c++-packages 'srefactor))

(defun c-c++/init-cc-mode ()
  (use-package cc-mode
    :defer t
    :init
    (add-to-list 'auto-mode-alist `("\\.h$" . ,c-c++-default-mode-for-headers))
    :config
    (progn
      (require 'compile)
      (c-toggle-auto-newline 1)
      (spacemacs/set-leader-keys-for-major-mode 'c-mode
        "ga" 'projectile-find-other-file
        "gA" 'projectile-find-other-file-other-window
        "gh" 'ff-find-other-file
        "c" 'projectile-compile-project
        "p" 'projectile-run-project)
      (spacemacs/set-leader-keys-for-major-mode 'c++-mode
        "ga" 'projectile-find-other-file
        "gA" 'projectile-find-other-file-other-window
        "gh" 'ff-find-other-file
        "c" 'projectile-compile-project
        "p" 'projectile-run-project))))

(defun c-c++/init-disaster ()
  (use-package disaster
    :defer t
    :commands (disaster)
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'c-mode
        "D" 'disaster)
      (spacemacs/set-leader-keys-for-major-mode 'c++-mode
        "D" 'disaster))))

(defun c-c++/init-clang-format ()
  (use-package clang-format
    :if c-c++-enable-clang-support))

(defun c-c++/init-cmake-mode ()
  (use-package cmake-mode
    :mode (("CMakeLists\\.txt\\'" . cmake-mode) ("\\.cmake\\'" . cmake-mode))
    :init (push 'company-cmake company-backends-cmake-mode)))

(defun c-c++/init-irony ()
  (use-package irony 
    :diminish irony-mode
    :defer t
    :init
    (progn
      (add-hook 'c++-mode-hook 'irony-mode)
      (add-hook 'c-mode-hook 'irony-mode)
      ;;see https://github.com/Sarcasm/irony-mode/issues/154#issuecomment-100649914
      ;;just use .clang_complete from now on
      ;; cannnot support json format. it is unstable at <2015-05-11 一>


      ;; replace the 'completion at point ' and 'complete-symbol' bindings in
      ;; irony mode's buffers ny irony-mode's function
      (defun my-irony-mode-hook ()
        (define-key irony-mode-map [remap completion-at-point]
          'irony-completion-at-point-async)
        (define-key irony-mode-map [remap complete-symbol]
          'irony-completion-at-point-async))
      (add-hook 'irony-mode-hook 'my-irony-mode-hook)
      (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
      (spacemacs|diminish irony-mode " Ⓘ" " I"))))

(defun c-c++/init-company-irony ()
  (use-package company-irony
    :defer t)
  (push '(company-irony company-yasnippet company-keywords company-gtags) company-backends-c-mode-common)
)

(defun c-c++/post-init-company ()
  (spacemacs|add-company-hook c-mode-common)
  (spacemacs|add-company-hook cmake-mode)

  (when c-c++-enable-clang-support
    (push 'company-clang company-backends-c-mode-common)

    (defun company-mode/more-than-prefix-guesser ()
      (c-c++/load-clang-args)
      (company-clang-guess-prefix))

    (setq company-clang-prefix-guesser 'company-mode/more-than-prefix-guesser)
    (spacemacs/add-to-hooks 'c-c++/load-clang-args '(c-mode-hook c++-mode-hook)))
  )

(when (configuration-layer/layer-usedp 'auto-completion)
  (defun c-c++/init-company-c-headers ()
    (use-package company-c-headers
      :if (configuration-layer/package-usedp 'company)
      :defer t
      :init (push 'company-c-headers company-backends-c-mode-common))))

(defun c-c++/post-init-flycheck ()
  (dolist (hook '(c-mode-hook c++-mode-hook))
    (spacemacs/add-flycheck-hook hook))
  (when c-c++-enable-clang-support
    (spacemacs/add-to-hooks 'c-c++/load-clang-args '(c-mode-hook c++-mode-hook))))

(defun c-c++/init-gdb-mi ()
  (use-package gdb-mi
    :defer t
    :init
    (setq
     ;; use gdb-many-windows by default when `M-x gdb'
     gdb-many-windows t
     ;; Non-nil means display source file containing the main routine at startup
     gdb-show-main t)))

(defun c-c++/post-init-helm-gtags ()
  (spacemacs/helm-gtags-define-keys-for-mode 'c-mode)
  (spacemacs/helm-gtags-define-keys-for-mode 'c++-mode)
  (define-key evil-normal-state-map (kbd "M-,") 'helm-gtags-dwim))

(defun c-c++/post-init-semantic ()
  (semantic/enable-semantic-mode 'c-mode)
  (semantic/enable-semantic-mode 'c++-mode)
  (spacemacs/set-leader-keys-for-major-mode 'c-mode "i" 'helm-semantic)
  (spacemacs/set-leader-keys-for-major-mode 'c++-mode "i" 'helm-semantic)
  )

(defun c-c++/post-init-srefactor ()
  (spacemacs/set-leader-keys-for-major-mode 'c-mode "r" 'srefactor-refactor-at-point)
  (spacemacs/set-leader-keys-for-major-mode 'c++-mode "r" 'srefactor-refactor-at-point)
  (spacemacs/add-to-hooks 'spacemacs/lazy-load-srefactor '(c-mode-hook c++-mode-hook)))

(defun c-c++/post-init-stickyfunc-enhance ()
  (spacemacs/add-to-hooks 'spacemacs/lazy-load-stickyfunc-enhance '(c-mode-hook c++-mode-hook)))

;; (defun c-c++/post-init-ycmd ()
;;   (add-hook 'c++-mode-hook 'ycmd-mode)
;;   (spacemacs/set-leader-keys-for-major-mode 'c++-mode
;;     "gg" 'ycmd-goto
;;     "gG" 'ycmd-goto-imprecise)
;;   (define-key evil-normal-state-map (kbd "M-.") 'ycmd-goto)
;;   )

;; (defun c-c++/post-init-company-ycmd ()
;;   ;; (push '(company-dabbrev-code company-keywords company-yasnippet company-ycmd company-clang) company-backends-c-mode-common))
;;   (push 'company-ycmd company-backends-c-mode-common))
;;   ;; (push '(company-gtags company-keywords company-yasnippet company-ycmd) company-backends-c-mode-common))

(defun c-c++/pre-init-xcscope ()
  (spacemacs|use-package-add-hook xcscope
    :post-init
    (dolist (mode '(c-mode c++-mode))
      (spacemacs/set-leader-keys-for-major-mode mode "gi" 'cscope-index-files))))

(defun c-c++/pre-init-helm-cscope ()
  (spacemacs|use-package-add-hook xcscope
    :post-init
    (dolist (mode '(c-mode c++-mode))
      (spacemacs/setup-helm-cscope mode))))
