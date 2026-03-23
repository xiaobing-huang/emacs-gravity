;;; claude-gravity-faces.el --- Face definitions for Claude Gravity  -*- lexical-binding: t; -*-

;;; Code:

(require 'claude-gravity-core)


;;; Face specs (shared between defface and face-spec-set for reload safety)

(defconst claude-gravity--face-specs
  '((claude-gravity-tool-done
     . ((t :inherit success)))
    (claude-gravity-tool-running
     . ((t :inherit warning)))
    (claude-gravity-tool-error
     . ((t :inherit error)))
    (claude-gravity-tool-name
     . ((t :weight bold)))
    (claude-gravity-detail-label
     . ((t :inherit shadow)))
    (claude-gravity-stderr
     . ((t :inherit error)))
    (claude-gravity-session-ended
     . ((t :inherit shadow)))
    (claude-gravity-prompt
     . ((((class color) (background dark))  :foreground "#00d3d0")
        (((class color) (background light)) :foreground "#005e8b")
        (t :foreground "cyan")))
    (claude-gravity-task-done
     . ((t :inherit success)))
    (claude-gravity-task-in-progress
     . ((t :inherit warning)))
    (claude-gravity-task-pending
     . ((t :inherit shadow)))
    (claude-gravity-task-active-form
     . ((t :inherit shadow :slant italic)))
    (claude-gravity-status-responding
     . ((t :inherit warning :weight bold)))
    (claude-gravity-status-idle
     . ((t :inherit success :weight bold)))
    (claude-gravity-file-ops
     . ((t :inherit shadow)))
    (claude-gravity-question
     . ((((class color) (background dark))  :foreground "#feacd0")
        (((class color) (background light)) :foreground "#721045")
        (t :foreground "magenta")))
    (claude-gravity-tool-signature
     . ((t :inherit shadow :slant italic)))
    (claude-gravity-tool-description
     . ((((class color) (background dark))  :foreground "#88cc88")
        (((class color) (background light)) :foreground "#006800")))
    (claude-gravity-assistant-text
     . ((((class color) (background dark))  :foreground "#ffbb66")
        (((class color) (background light)) :foreground "#9a5000")))
    (claude-gravity-agent-stop-text
     . ((((class color) (background dark))  :foreground "#88ccaa")
        (((class color) (background light)) :foreground "#00663f")))
    (claude-gravity-thinking
     . ((((class color) (background dark))  :foreground "#d0a0ff" :slant italic)
        (((class color) (background light)) :foreground "#7030a0" :slant italic)
        (t :slant italic)))
    (claude-gravity-section-heading
     . ((((class color) (background dark))  :weight bold :foreground "white")
        (((class color) (background light)) :weight bold :foreground unspecified)
        (t :weight bold)))
    (claude-gravity-divider
     . ((((background dark))  :foreground "gray40")
        (((background light)) :foreground "gray60")))
    (claude-gravity-margin-indicator
     . ((((background dark))  :foreground "gray40")
        (((background light)) :foreground "gray60")))
    (claude-gravity-running-bg
     . ((((background dark)) :background "#2a2a00")
        (((background light)) :background "#fffde0")))
    (claude-gravity-agent-bg
     . ((((background dark)) :background "#0a1a2a")
        (((background light)) :background "#f0f5fa")))
    (claude-gravity-agent-nested-bg
     . ((((background dark)) :background "#0f2030")
        (((background light)) :background "#e8f0f5")))
    (claude-gravity-agent-margin
     . ((((class color) (background dark))  :foreground "#5599aa")
        (((class color) (background light)) :foreground "#005f5f")))
    (claude-gravity-diff-added
     . ((((background dark)) :foreground "#88ee88" :background "#1a3a1a")
        (((background light)) :foreground "#006600" :background "#ddffdd")))
    (claude-gravity-diff-removed
     . ((((background dark)) :foreground "#ee8888" :background "#3a1a1a" :strike-through nil)
        (((background light)) :foreground "#660000" :background "#ffdddd" :strike-through nil)))
    (claude-gravity-diff-context
     . ((((background dark)) :foreground "#888888")
        (((background light)) :foreground "#666666")))
    (claude-gravity-diff-header
     . ((((background dark)) :foreground "#7799cc")
        (((background light)) :foreground "#336699")))
    (claude-gravity-plan-margin-added
     . ((((background dark)) :foreground "#88ee88")
        (((background light)) :foreground "#22aa22")))
    (claude-gravity-plan-margin-modified
     . ((((background dark)) :foreground "#eeaa44")
        (((background light)) :foreground "#cc8800")))
    (claude-gravity-plan-margin-deleted
     . ((((background dark)) :foreground "#ee8888")
        (((background light)) :foreground "#cc4444")))
    (claude-gravity-phase-boundary
     . ((((background dark)) :foreground "#66dd66" :background "#2a2a00")
        (((background light)) :foreground "#228822" :background "#ffffdd")))
    (claude-gravity-header-title
     . ((((class color) (background dark))  :weight bold :foreground "white")
        (((class color) (background light)) :weight bold :foreground unspecified)
        (t :weight bold)))
    (claude-gravity-slug
     . ((t :inherit shadow :slant italic)))
    (claude-gravity-branch
     . ((((class color) (background dark))  :foreground "#5fafaf")
        (((class color) (background light)) :foreground "#2e8b8b")
        (t :foreground "cyan")))
    (claude-gravity-comment-overlay
     . ((((class color) (background dark))  :foreground "orange" :slant italic)
        (((class color) (background light)) :foreground "#a0522d" :slant italic)
        (t :slant italic)))
    (claude-gravity-comment-underline
     . ((((class color) (background dark))  :underline (:style wave :color "orange"))
        (((class color) (background light)) :underline (:style wave :color "#a0522d"))
        (t :underline t))))
  "Alist of (face-symbol . face-spec) for all claude-gravity faces.
Used by both `defface' and `claude-gravity-reset-faces' to ensure
face specs are applied even when the file is reloaded into a running
Emacs (since `defface' does not override existing face definitions).")


;;; Faces

(defface claude-gravity-tool-done
  (cdr (assq 'claude-gravity-tool-done claude-gravity--face-specs))
  "Face for completed tool status indicator."
  :group 'claude-gravity)

(defface claude-gravity-tool-running
  (cdr (assq 'claude-gravity-tool-running claude-gravity--face-specs))
  "Face for running tool status indicator."
  :group 'claude-gravity)

(defface claude-gravity-tool-error
  (cdr (assq 'claude-gravity-tool-error claude-gravity--face-specs))
  "Face for failed tool status indicator."
  :group 'claude-gravity)

(defface claude-gravity-tool-name
  (cdr (assq 'claude-gravity-tool-name claude-gravity--face-specs))
  "Face for tool name."
  :group 'claude-gravity)

(defface claude-gravity-detail-label
  (cdr (assq 'claude-gravity-detail-label claude-gravity--face-specs))
  "Face for detail labels in expanded tool view."
  :group 'claude-gravity)

(defface claude-gravity-stderr
  (cdr (assq 'claude-gravity-stderr claude-gravity--face-specs))
  "Face for stderr output."
  :group 'claude-gravity)

(defface claude-gravity-session-ended
  (cdr (assq 'claude-gravity-session-ended claude-gravity--face-specs))
  "Face for ended session indicator."
  :group 'claude-gravity)

(defface claude-gravity-prompt
  (cdr (assq 'claude-gravity-prompt claude-gravity--face-specs))
  "Face for user prompt text."
  :group 'claude-gravity)

(defface claude-gravity-task-done
  (cdr (assq 'claude-gravity-task-done claude-gravity--face-specs))
  "Face for completed task checkbox."
  :group 'claude-gravity)

(defface claude-gravity-task-in-progress
  (cdr (assq 'claude-gravity-task-in-progress claude-gravity--face-specs))
  "Face for in-progress task checkbox."
  :group 'claude-gravity)

(defface claude-gravity-task-pending
  (cdr (assq 'claude-gravity-task-pending claude-gravity--face-specs))
  "Face for pending task checkbox."
  :group 'claude-gravity)

(defface claude-gravity-task-active-form
  (cdr (assq 'claude-gravity-task-active-form claude-gravity--face-specs))
  "Face for task activeForm text."
  :group 'claude-gravity)

(defface claude-gravity-status-responding
  (cdr (assq 'claude-gravity-status-responding claude-gravity--face-specs))
  "Face for responding status."
  :group 'claude-gravity)

(defface claude-gravity-status-idle
  (cdr (assq 'claude-gravity-status-idle claude-gravity--face-specs))
  "Face for idle status."
  :group 'claude-gravity)

(defface claude-gravity-file-ops
  (cdr (assq 'claude-gravity-file-ops claude-gravity--face-specs))
  "Face for file operation labels."
  :group 'claude-gravity)

(defface claude-gravity-question
  (cdr (assq 'claude-gravity-question claude-gravity--face-specs))
  "Face for AskUserQuestion prompt indicators."
  :group 'claude-gravity)

(defface claude-gravity-tool-signature
  (cdr (assq 'claude-gravity-tool-signature claude-gravity--face-specs))
  "Face for tool permission signature text."
  :group 'claude-gravity)

(defface claude-gravity-tool-description
  (cdr (assq 'claude-gravity-tool-description claude-gravity--face-specs))
  "Face for tool description text (the human-readable intent)."
  :group 'claude-gravity)

(defface claude-gravity-assistant-text
  (cdr (assq 'claude-gravity-assistant-text claude-gravity--face-specs))
  "Face for assistant monologue text between tool calls."
  :group 'claude-gravity)

(defface claude-gravity-agent-stop-text
  (cdr (assq 'claude-gravity-agent-stop-text claude-gravity--face-specs))
  "Face for agent completion summary text (SubagentStop)."
  :group 'claude-gravity)

(defface claude-gravity-thinking
  (cdr (assq 'claude-gravity-thinking claude-gravity--face-specs))
  "Face for assistant extended thinking text."
  :group 'claude-gravity)

(defface claude-gravity-section-heading
  (cdr (assq 'claude-gravity-section-heading claude-gravity--face-specs))
  "Face for major section heading text (bold; no foreground override on light themes)."
  :group 'claude-gravity)

(defface claude-gravity-divider
  (cdr (assq 'claude-gravity-divider claude-gravity--face-specs))
  "Face for section and turn separator lines."
  :group 'claude-gravity)

(defface claude-gravity-margin-indicator
  (cdr (assq 'claude-gravity-margin-indicator claude-gravity--face-specs))
  "Fallback face for margin block indicators (▎).
Content-type faces (thinking, assistant-text, detail-label) are
preferred at call sites; this face is used when no content face
is provided."
  :group 'claude-gravity)

(defface claude-gravity-running-bg
  (cdr (assq 'claude-gravity-running-bg claude-gravity--face-specs))
  "Subtle background highlight for running tools/agents."
  :group 'claude-gravity)

(defface claude-gravity-agent-bg
  (cdr (assq 'claude-gravity-agent-bg claude-gravity--face-specs))
  "Subtle background tint for agent sub-branch content."
  :group 'claude-gravity)

(defface claude-gravity-agent-nested-bg
  (cdr (assq 'claude-gravity-agent-nested-bg claude-gravity--face-specs))
  "Background for nested agent sub-branches (2+ levels deep)."
  :group 'claude-gravity)

(defface claude-gravity-agent-margin
  (cdr (assq 'claude-gravity-agent-margin claude-gravity--face-specs))
  "Fallback face for margin indicator inside agent response steps.
Content-type faces are preferred; agent distinction is provided
by the background tint (`claude-gravity-agent-bg')."
  :group 'claude-gravity)

(defface claude-gravity-diff-added
  (cdr (assq 'claude-gravity-diff-added claude-gravity--face-specs))
  "Face for added text in inline Edit diffs."
  :group 'claude-gravity)

(defface claude-gravity-diff-removed
  (cdr (assq 'claude-gravity-diff-removed claude-gravity--face-specs))
  "Face for removed text in inline Edit diffs."
  :group 'claude-gravity)

(defface claude-gravity-diff-context
  (cdr (assq 'claude-gravity-diff-context claude-gravity--face-specs))
  "Face for context lines in unified-style Edit diffs."
  :group 'claude-gravity)

(defface claude-gravity-diff-header
  (cdr (assq 'claude-gravity-diff-header claude-gravity--face-specs))
  "Face for @@ hunk headers in Edit diffs."
  :group 'claude-gravity)

(defface claude-gravity-plan-margin-added
  (cdr (assq 'claude-gravity-plan-margin-added claude-gravity--face-specs))
  "Fringe face for added lines in plan revision diff."
  :group 'claude-gravity)

(defface claude-gravity-plan-margin-modified
  (cdr (assq 'claude-gravity-plan-margin-modified claude-gravity--face-specs))
  "Fringe face for modified lines in plan revision diff."
  :group 'claude-gravity)

(defface claude-gravity-plan-margin-deleted
  (cdr (assq 'claude-gravity-plan-margin-deleted claude-gravity--face-specs))
  "Fringe face for deleted-region markers in plan revision diff."
  :group 'claude-gravity)

(defface claude-gravity-phase-boundary
  (cdr (assq 'claude-gravity-phase-boundary claude-gravity--face-specs))
  "Face for plan-approved phase boundary prompts."
  :group 'claude-gravity)

(defface claude-gravity-header-title
  (cdr (assq 'claude-gravity-header-title claude-gravity--face-specs))
  "Face for the main buffer header title (bold; no foreground override on light themes)."
  :group 'claude-gravity)

(defface claude-gravity-slug
  (cdr (assq 'claude-gravity-slug claude-gravity--face-specs))
  "Face for the session slug shown in the header."
  :group 'claude-gravity)

(defface claude-gravity-branch
  (cdr (assq 'claude-gravity-branch claude-gravity--face-specs))
  "Face for git branch name in session list and header."
  :group 'claude-gravity)

(defface claude-gravity-comment-overlay
  (cdr (assq 'claude-gravity-comment-overlay claude-gravity--face-specs))
  "Face for comment text in plan/annotation overlays."
  :group 'claude-gravity)

(defface claude-gravity-comment-underline
  (cdr (assq 'claude-gravity-comment-underline claude-gravity--face-specs))
  "Face for wave underline in plan/annotation comment overlays."
  :group 'claude-gravity)


;; Force-apply specs on load — defface does not override existing face
;; definitions on reload (same behavior as defvar).
(dolist (entry claude-gravity--face-specs)
  (face-spec-set (car entry) (cdr entry)))


(when (display-graphic-p)
  (define-fringe-bitmap 'claude-gravity-plan-added
    [#b00011100] nil nil '(center repeated))
  (define-fringe-bitmap 'claude-gravity-plan-modified
    [#b00011100] nil nil '(center repeated))
  (define-fringe-bitmap 'claude-gravity-plan-deleted
    [#b00010000
     #b00011000
     #b00011100
     #b00011000
     #b00010000] nil nil '(center t)))

(provide 'claude-gravity-faces)
;;; claude-gravity-faces.el ends here
