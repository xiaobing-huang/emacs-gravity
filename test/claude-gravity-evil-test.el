;;; claude-gravity-evil-test.el --- ERT tests for Evil mode integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests that verify claude-gravity-evil.el correctly registers Evil bindings.
;; These tests require Evil to be available.  They are skipped when evil is
;; not loaded.

;;; Code:

(require 'ert)

(when (require 'evil nil t)
  (require 'claude-gravity)
  (require 'claude-gravity-evil)

  (defun cg-evil-test--normal-lookup (map key)
    "Look up KEY in MAP's Evil normal-state auxiliary keymap.
Returns the bound command, or nil if not bound."
    (let ((aux (evil-get-auxiliary-keymap map 'normal)))
      (when aux
        (lookup-key aux (if (stringp key) (kbd key) key)))))

  (defun cg-evil-test--state-lookup (state map key)
    "Look up KEY in MAP's Evil STATE auxiliary keymap."
    (let ((aux (evil-get-auxiliary-keymap map state)))
      (when aux
        (lookup-key aux (if (stringp key) (kbd key) key)))))


  ;;; Initial states

  (ert-deftest cg-evil-initial-state-overview ()
    "Overview mode starts in Evil normal state."
    (should (eq (evil-initial-state 'claude-gravity-mode) 'normal)))

  (ert-deftest cg-evil-initial-state-session ()
    "Session mode starts in Evil normal state."
    (should (eq (evil-initial-state 'claude-gravity-session-mode) 'normal)))

  (ert-deftest cg-evil-initial-state-debug ()
    "Debug mode starts in Evil normal state."
    (should (eq (evil-initial-state 'claude-gravity-debug-mode) 'normal)))

  (ert-deftest cg-evil-initial-state-popup ()
    "Popup mode starts in Evil normal state."
    (should (eq (evil-initial-state 'claude-gravity-popup-mode) 'normal)))


  ;;; Navigation aliases

  (ert-deftest cg-evil-navigation-j ()
    "j is bound to section-forward in normal state."
    (should (eq (cg-evil-test--normal-lookup claude-gravity-mode-map "j")
                #'claude-gravity--section-forward)))

  (ert-deftest cg-evil-navigation-k ()
    "k is bound to section-backward in normal state."
    (should (eq (cg-evil-test--normal-lookup claude-gravity-mode-map "k")
                #'claude-gravity--section-backward)))

  (ert-deftest cg-evil-navigation-n ()
    "n is also bound to section-forward."
    (should (eq (cg-evil-test--normal-lookup claude-gravity-mode-map "n")
                #'claude-gravity--section-forward)))

  (ert-deftest cg-evil-navigation-p ()
    "p is also bound to section-backward."
    (should (eq (cg-evil-test--normal-lookup claude-gravity-mode-map "p")
                #'claude-gravity--section-backward)))


  ;;; Refresh uses gr, not bare g

  (ert-deftest cg-evil-refresh-gr ()
    "gr is bound to refresh."
    (should (eq (cg-evil-test--normal-lookup claude-gravity-mode-map "gr")
                #'claude-gravity-refresh)))

  (ert-deftest cg-evil-no-bare-g-binding ()
    "Bare g is NOT bound in normal state — preserves gg, gj, etc."
    ;; The binding for bare "g" should be nil in our auxiliary keymap.
    ;; (Evil's own g-prefix motions are in evil-normal-state-map, not here.)
    (let ((aux (evil-get-auxiliary-keymap claude-gravity-mode-map 'normal)))
      (when aux
        (should-not (eq (lookup-key aux "g") #'claude-gravity-refresh)))))

  (ert-deftest cg-evil-force-resync-gR ()
    "gR is bound to force-resync."
    (should (eq (cg-evil-test--normal-lookup claude-gravity-mode-map "gR")
                #'claude-gravity-force-resync)))


  ;;; Key bindings in overview mode

  (ert-deftest cg-evil-overview-quit ()
    "q is bound to quit-window."
    (should (eq (cg-evil-test--normal-lookup claude-gravity-mode-map "q")
                #'quit-window)))

  (ert-deftest cg-evil-overview-K-dismiss ()
    "K is bound to inbox-dismiss (capital K, not lowercase k)."
    (should (eq (cg-evil-test--normal-lookup claude-gravity-mode-map "K")
                #'claude-gravity-inbox-dismiss)))

  (ert-deftest cg-evil-overview-ret ()
    "RET is bound to a lambda (visit-or-toggle)."
    (let ((binding (cg-evil-test--normal-lookup claude-gravity-mode-map (kbd "RET"))))
      (should (functionp binding))))

  (ert-deftest cg-evil-overview-tab ()
    "TAB is bound to magit-section-toggle."
    (should (eq (cg-evil-test--normal-lookup claude-gravity-mode-map (kbd "TAB"))
                #'magit-section-toggle)))

  (ert-deftest cg-evil-overview-prefix-i ()
    "i is bound to the inbox prefix keymap."
    (let ((binding (cg-evil-test--normal-lookup claude-gravity-mode-map "i")))
      (should (keymapp binding))))

  (ert-deftest cg-evil-overview-prefix-S ()
    "S is bound to the session-cmd prefix keymap."
    (let ((binding (cg-evil-test--normal-lookup claude-gravity-mode-map "S")))
      (should (keymapp binding))))


  ;;; Session mode

  (ert-deftest cg-evil-session-o ()
    "o is bound to return-to-overview in session mode."
    (should (eq (cg-evil-test--normal-lookup claude-gravity-session-mode-map "o")
                #'claude-gravity-return-to-overview)))

  (ert-deftest cg-evil-session-spc ()
    "SPC is bound to popup-at-point in session mode."
    (should (eq (cg-evil-test--normal-lookup claude-gravity-session-mode-map (kbd "SPC"))
                #'claude-gravity-popup-at-point)))


  ;;; Permission action mode

  (ert-deftest cg-evil-permission-a ()
    "a is bound to permission-action-allow."
    (should (eq (cg-evil-test--normal-lookup claude-gravity-permission-action-mode-map "a")
                #'claude-gravity-permission-action-allow)))

  (ert-deftest cg-evil-permission-d ()
    "d is bound to permission-action-deny."
    (should (eq (cg-evil-test--normal-lookup claude-gravity-permission-action-mode-map "d")
                #'claude-gravity-permission-action-deny)))

  (ert-deftest cg-evil-permission-q ()
    "q is bound to permission-action-quit."
    (should (eq (cg-evil-test--normal-lookup claude-gravity-permission-action-mode-map "q")
                #'claude-gravity-permission-action-quit)))


  ;;; Question action mode

  (ert-deftest cg-evil-question-j ()
    "j is bound to question-focus-next."
    (should (eq (cg-evil-test--normal-lookup claude-gravity-question-action-mode-map "j")
                #'claude-gravity-question-focus-next)))

  (ert-deftest cg-evil-question-k ()
    "k is bound to question-focus-prev."
    (should (eq (cg-evil-test--normal-lookup claude-gravity-question-action-mode-map "k")
                #'claude-gravity-question-focus-prev)))

  (ert-deftest cg-evil-question-1 ()
    "1 is bound to question-action-1."
    (should (eq (cg-evil-test--normal-lookup claude-gravity-question-action-mode-map "1")
                #'claude-gravity-question-action-1)))

  (ert-deftest cg-evil-question-submit ()
    "C-c C-c is bound to question-submit."
    (should (eq (cg-evil-test--normal-lookup claude-gravity-question-action-mode-map (kbd "C-c C-c"))
                #'claude-gravity-question-submit)))


  ;;; Plan review mode

  (ert-deftest cg-evil-plan-review-approve ()
    "C-c C-c is bound to plan-review-approve in normal state."
    (should (eq (cg-evil-test--normal-lookup claude-gravity-plan-review-mode-map (kbd "C-c C-c"))
                #'claude-gravity-plan-review-approve)))

  (ert-deftest cg-evil-plan-review-deny ()
    "C-c C-k is bound to plan-review-deny in normal state."
    (should (eq (cg-evil-test--normal-lookup claude-gravity-plan-review-mode-map (kbd "C-c C-k"))
                #'claude-gravity-plan-review-deny)))


  ;;; Compose mode

  (ert-deftest cg-evil-compose-hook ()
    "claude-gravity-compose-mode-hook includes evil-insert-state."
    (should (memq #'evil-insert-state claude-gravity-compose-mode-hook)))

  (ert-deftest cg-evil-compose-send-normal ()
    "C-c C-c is bound to compose-send in normal state."
    (should (eq (cg-evil-test--state-lookup 'normal claude-gravity-compose-mode-map (kbd "C-c C-c"))
                #'claude-gravity-compose-send)))

  (ert-deftest cg-evil-compose-send-insert ()
    "C-c C-c is bound to compose-send in insert state."
    (should (eq (cg-evil-test--state-lookup 'insert claude-gravity-compose-mode-map (kbd "C-c C-c"))
                #'claude-gravity-compose-send)))


  ;;; Popup mode

  (ert-deftest cg-evil-popup-q ()
    "q is bound to popup-dismiss."
    (should (eq (cg-evil-test--normal-lookup claude-gravity-popup-mode-map "q")
                #'claude-gravity-popup-dismiss)))


  ;;; Debug mode

  (ert-deftest cg-evil-debug-gr ()
    "gr is bound to debug-refresh."
    (should (eq (cg-evil-test--normal-lookup claude-gravity-debug-mode-map "gr")
                #'claude-gravity-debug-refresh)))

  (ert-deftest cg-evil-debug-j ()
    "j is bound to next-line in debug mode."
    (should (eq (cg-evil-test--normal-lookup claude-gravity-debug-mode-map "j")
                #'next-line)))


  ;;; Snipe disable hooks

  (ert-deftest cg-evil-snipe-disable-hook-overview ()
    "evil-snipe disable hook is registered for overview mode."
    (should (memq #'claude-gravity--evil-disable-snipe
                  claude-gravity-mode-hook)))

  (ert-deftest cg-evil-snipe-disable-hook-session ()
    "evil-snipe disable hook is registered for session mode."
    (should (memq #'claude-gravity--evil-disable-snipe
                  claude-gravity-session-mode-hook)))

  (ert-deftest cg-evil-snipe-disable-hook-debug ()
    "evil-snipe disable hook is registered for debug mode."
    (should (memq #'claude-gravity--evil-disable-snipe
                  claude-gravity-debug-mode-hook)))

  (ert-deftest cg-evil-snipe-disable-hook-popup ()
    "evil-snipe disable hook is registered for popup mode."
    (should (memq #'claude-gravity--evil-disable-snipe
                  claude-gravity-popup-mode-hook)))

  )  ;; end (when (require 'evil nil t) ...)

(provide 'claude-gravity-evil-test)
;;; claude-gravity-evil-test.el ends here
