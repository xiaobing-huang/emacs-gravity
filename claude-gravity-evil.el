;;; claude-gravity-evil.el --- Evil mode integration for claude-gravity  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  User
;;
;; This file is loaded automatically when evil is available.
;; It registers all gravity keybindings into Evil's state-specific
;; auxiliary keymaps so they work in normal state without conflicts.

;;; Code:

(require 'evil)
(require 'claude-gravity-core)

;; Forward-declare keymaps (defined in their respective modules)
(defvar claude-gravity-mode-map)
(defvar claude-gravity-session-mode-map)
(defvar claude-gravity-debug-mode-map)
(defvar claude-gravity-popup-mode-map)
(defvar claude-gravity-permission-action-mode-map)
(defvar claude-gravity-question-action-mode-map)
(defvar claude-gravity-plan-review-mode-map)
(defvar claude-gravity-compose-mode-map)
(defvar claude-gravity-inbox-map)
(defvar claude-gravity-session-cmd-map)


;;; ============================================================================
;;; Section 1: Initial Evil States for Major Modes
;;; ============================================================================

;; All gravity major modes use read-only section-style navigation.
;; Use normal state so j/k and all single-letter bindings work immediately.
(evil-set-initial-state 'claude-gravity-mode 'normal)
(evil-set-initial-state 'claude-gravity-session-mode 'normal)
(evil-set-initial-state 'claude-gravity-debug-mode 'normal)
(evil-set-initial-state 'claude-gravity-popup-mode 'normal)


;;; ============================================================================
;;; Section 2: Overview and Session Base Map (claude-gravity-mode-map)
;;; ============================================================================

(evil-define-key* 'normal claude-gravity-mode-map
  ;; Navigation — j/k as vi-style aliases for n/p
  "j"           #'claude-gravity--section-forward
  "k"           #'claude-gravity--section-backward
  "n"           #'claude-gravity--section-forward
  "p"           #'claude-gravity--section-backward

  ;; Refresh — gr/gR follow evil-collection-magit convention;
  ;; avoids shadowing Evil's gg/gj/gk g-prefix motions.
  "gr"          #'claude-gravity-refresh
  "gR"          #'claude-gravity-force-resync

  ;; Core actions
  "c"           #'claude-gravity-comment-at-point
  "P"           #'claude-gravity-show-plan
  "?"           #'claude-gravity-overview-menu
  (kbd "TAB")   #'magit-section-toggle
  (kbd "RET")   (lambda ()
                  "Visit or toggle section, but only when on a valid section."
                  (interactive)
                  (let ((section (magit-current-section)))
                    (when section
                      (claude-gravity-visit-or-toggle))))

  ;; Session management
  "D"           #'claude-gravity-cleanup-sessions
  "R"           #'claude-gravity-reset-status
  "X"           #'claude-gravity-detect-dead-sessions
  "d"           #'claude-gravity-delete-session

  ;; Allow patterns
  "A"           #'claude-gravity-add-allow-pattern
  "a"           #'claude-gravity-add-allow-pattern-to-settings

  ;; Plan
  "F"           #'claude-gravity-open-plan-file

  ;; Session controls
  "t"           #'claude-gravity-tail
  "f"           #'claude-gravity-follow-mode
  "e"           #'claude-gravity-edit-entry
  "b"           #'claude-gravity-switch-session

  ;; Inbox — K (capital) for dismiss to preserve k=up navigation
  "K"           #'claude-gravity-inbox-dismiss

  "w"           #'claude-gravity-copy-section

  ;; Prefix submaps — Evil handles keymaps-as-values correctly
  "i"           claude-gravity-inbox-map
  "S"           claude-gravity-session-cmd-map

  ;; Transcripts
  "T"           #'claude-gravity-view-agent-transcript
  "V"           #'claude-gravity-open-agent-transcript

  ;; Compose / Debug
  "s"           #'claude-gravity-unified-compose
  "M"           #'claude-gravity-debug-show

  ;; Standard Evil quit
  "q"           #'quit-window)


;;; ============================================================================
;;; Section 3: Session Detail Map (claude-gravity-session-mode-map)
;;; ============================================================================

(evil-define-key* 'normal claude-gravity-session-mode-map
  "o"           #'claude-gravity-return-to-overview
  "l"           #'claude-gravity-set-permission-mode
  "?"           #'claude-gravity-session-menu
  (kbd "SPC")   #'claude-gravity-popup-at-point)


;;; ============================================================================
;;; Section 4: Debug Buffer (claude-gravity-debug-mode-map)
;;; ============================================================================

(evil-define-key* 'normal claude-gravity-debug-mode-map
  "j"           #'next-line
  "k"           #'previous-line
  "gr"          #'claude-gravity-debug-refresh
  "c"           #'claude-gravity-debug-copy-raw
  "C"           #'claude-gravity-debug-copy-parsed
  (kbd "RET")   #'claude-gravity-debug-toggle-expand
  "f"           #'claude-gravity-debug-filter-type
  "s"           #'claude-gravity-debug-filter-session
  "d"           #'claude-gravity-debug-filter-direction
  "p"           #'claude-gravity-debug-filter-patch-op
  "/"           #'claude-gravity-debug-search
  "x"           #'claude-gravity-debug-clear
  "q"           #'quit-window)


;;; ============================================================================
;;; Section 5: Popup Buffer (claude-gravity-popup-mode-map)
;;; ============================================================================

(evil-define-key* 'normal claude-gravity-popup-mode-map
  "q"           #'claude-gravity-popup-dismiss
  (kbd "SPC")   #'claude-gravity-popup-dismiss)


;;; ============================================================================
;;; Section 6: Permission Action Minor Mode
;;; ============================================================================

(evil-define-key* 'normal claude-gravity-permission-action-mode-map
  "a"           #'claude-gravity-permission-action-allow
  "A"           #'claude-gravity-permission-action-allow-always
  "S"           #'claude-gravity-permission-action-allow-with-permissions
  "d"           #'claude-gravity-permission-action-deny
  "p"           #'claude-gravity-permission-action-add-pattern
  "!"           #'claude-gravity-permission-action-allow-turn
  "q"           #'claude-gravity-permission-action-quit)


;;; ============================================================================
;;; Section 7: Question Action Minor Mode
;;; ============================================================================

(evil-define-key* 'normal claude-gravity-question-action-mode-map
  "1"           #'claude-gravity-question-action-1
  "2"           #'claude-gravity-question-action-2
  "3"           #'claude-gravity-question-action-3
  "4"           #'claude-gravity-question-action-4
  "o"           #'claude-gravity-question-action-other
  "q"           #'claude-gravity-question-action-quit
  "]"           #'claude-gravity-question-next-tab
  "["           #'claude-gravity-question-prev-tab
  (kbd "<tab>") #'claude-gravity-question-next-tab
  (kbd "<backtab>") #'claude-gravity-question-prev-tab
  ;; Navigation — j/k vi aliases + n/p original
  "j"           #'claude-gravity-question-focus-next
  "n"           #'claude-gravity-question-focus-next
  "k"           #'claude-gravity-question-focus-prev
  "p"           #'claude-gravity-question-focus-prev
  (kbd "RET")   #'claude-gravity-question-toggle-or-select
  (kbd "SPC")   #'claude-gravity-question-toggle-or-select
  (kbd "C-c C-c") #'claude-gravity-question-submit
  "v"           #'claude-gravity-question-preview-markdown)


;;; ============================================================================
;;; Section 8: Plan Review Minor Mode (editable)
;;; ============================================================================

;; Bind C-c prefix actions in normal state so they work without entering insert.
(evil-define-key* 'normal claude-gravity-plan-review-mode-map
  (kbd "C-c C-c") #'claude-gravity-plan-review-approve
  (kbd "C-c C-k") #'claude-gravity-plan-review-deny
  (kbd "C-c C-d") #'claude-gravity-plan-review-diff
  (kbd "C-c ;")   #'claude-gravity-plan-review-comment
  (kbd "C-c ?")   #'claude-gravity-plan-review-menu
  (kbd "C-c C-g") #'claude-gravity-plan-review-toggle-margins
  (kbd "C-c C-l") #'claude-gravity-plan-review-approve-and-clear)


;;; ============================================================================
;;; Section 9: Compose Minor Mode (editable — auto-enters insert state)
;;; ============================================================================

;; When the compose minor mode activates, switch to insert state so the user
;; can start typing immediately.  evil-set-initial-state only works for major
;; modes, so we use a hook on the minor mode instead.
(add-hook 'claude-gravity-compose-mode-hook #'evil-insert-state)

;; Bind send/cancel in both normal and insert state so they always work.
(evil-define-key* '(normal insert) claude-gravity-compose-mode-map
  (kbd "C-c C-c") #'claude-gravity-compose-send
  (kbd "C-c C-k") #'claude-gravity-compose-cancel)


;;; ============================================================================

(provide 'claude-gravity-evil)
;;; claude-gravity-evil.el ends here
