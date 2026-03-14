;;; claude-gravity-faces.el --- Face definitions for Claude Gravity  -*- lexical-binding: t; -*-

;;; Code:

(require 'claude-gravity-core)


;;; Faces

(defface claude-gravity-tool-done
  '((((background dark))  :foreground "green")
    (((background light)) :foreground "#006600"))
  "Face for completed tool status indicator."
  :group 'claude-gravity)


(defface claude-gravity-tool-running
  '((((background dark))  :foreground "yellow")
    (((background light)) :foreground "#996600"))
  "Face for running tool status indicator."
  :group 'claude-gravity)


(defface claude-gravity-tool-error
  '((((background dark))  :foreground "red")
    (((background light)) :foreground "#cc0000"))
  "Face for failed tool status indicator."
  :group 'claude-gravity)


(defface claude-gravity-tool-name
  '((t :weight bold))
  "Face for tool name."
  :group 'claude-gravity)


(defface claude-gravity-detail-label
  '((((background dark))  :foreground "gray60")
    (((background light)) :foreground "gray40"))
  "Face for detail labels in expanded tool view."
  :group 'claude-gravity)


(defface claude-gravity-stderr
  '((((background dark))  :foreground "red")
    (((background light)) :foreground "#cc0000"))
  "Face for stderr output."
  :group 'claude-gravity)


(defface claude-gravity-session-ended
  '((((background dark))  :foreground "gray60")
    (((background light)) :foreground "gray40"))
  "Face for ended session indicator."
  :group 'claude-gravity)


(defface claude-gravity-prompt
  '((((background dark))  :foreground "cyan")
    (((background light)) :foreground "#006699"))
  "Face for user prompt text."
  :group 'claude-gravity)


(defface claude-gravity-task-done
  '((((background dark))  :foreground "green")
    (((background light)) :foreground "#006600"))
  "Face for completed task checkbox."
  :group 'claude-gravity)


(defface claude-gravity-task-in-progress
  '((((background dark))  :foreground "yellow")
    (((background light)) :foreground "#996600"))
  "Face for in-progress task checkbox."
  :group 'claude-gravity)


(defface claude-gravity-task-pending
  '((((background dark))  :foreground "gray60")
    (((background light)) :foreground "gray40"))
  "Face for pending task checkbox."
  :group 'claude-gravity)


(defface claude-gravity-task-active-form
  '((((background dark))  :foreground "gray60" :slant italic)
    (((background light)) :foreground "gray40" :slant italic))
  "Face for task activeForm text."
  :group 'claude-gravity)


(defface claude-gravity-status-responding
  '((((background dark))  :foreground "yellow")
    (((background light)) :foreground "#996600"))
  "Face for responding status."
  :group 'claude-gravity)


(defface claude-gravity-status-idle
  '((((background dark))  :foreground "green")
    (((background light)) :foreground "#006600"))
  "Face for idle status."
  :group 'claude-gravity)


(defface claude-gravity-file-ops
  '((((background dark))  :foreground "gray60")
    (((background light)) :foreground "gray40"))
  "Face for file operation labels."
  :group 'claude-gravity)


(defface claude-gravity-question
  '((((background dark))  :foreground "magenta")
    (((background light)) :foreground "#880088"))
  "Face for AskUserQuestion prompt indicators."
  :group 'claude-gravity)


(defface claude-gravity-tool-signature
  '((((background dark))  :foreground "gray55" :slant italic)
    (((background light)) :foreground "gray35" :slant italic))
  "Face for tool permission signature text."
  :group 'claude-gravity)


(defface claude-gravity-tool-description
  '((((background dark))  :foreground "#88cc88")
    (((background light)) :foreground "#338833"))
  "Face for tool description text (the human-readable intent)."
  :group 'claude-gravity)


(defface claude-gravity-assistant-text
  '((((background dark))  :foreground "#ffbb66")
    (((background light)) :foreground "#aa6600"))
  "Face for assistant monologue text between tool calls."
  :group 'claude-gravity)


(defface claude-gravity-agent-stop-text
  '((((background dark))  :foreground "#88ccaa")
    (((background light)) :foreground "#337755"))
  "Face for agent completion summary text (SubagentStop)."
  :group 'claude-gravity)


(defface claude-gravity-thinking
  '((((background dark))  :foreground "#d0a0ff" :slant italic)
    (((background light)) :foreground "#6633aa" :slant italic))
  "Face for assistant extended thinking text."
  :group 'claude-gravity)


(defface claude-gravity-section-heading
  '((((background dark))  :weight bold :foreground "white")
    (((background light)) :weight bold :foreground "black"))
  "Face for major section heading text."
  :group 'claude-gravity)


(defface claude-gravity-divider
  '((((background dark))  :foreground "gray40")
    (((background light)) :foreground "gray60"))
  "Face for section and turn separator lines."
  :group 'claude-gravity)


(defface claude-gravity-margin-indicator
  '((((background dark))  :foreground "gray40")
    (((background light)) :foreground "gray60"))
  "Fallback face for margin block indicators (▎).
Content-type faces (thinking, assistant-text, detail-label) are
preferred at call sites; this face is used when no content face
is provided."
  :group 'claude-gravity)


(defface claude-gravity-running-bg
  '((((background dark)) :background "#2a2a00")
    (((background light)) :background "#fffde0"))
  "Subtle background highlight for running tools/agents."
  :group 'claude-gravity)


(defface claude-gravity-agent-bg
  '((((background dark)) :background "#0a1a2a")
    (((background light)) :background "#f0f5fa"))
  "Subtle background tint for agent sub-branch content."
  :group 'claude-gravity)


(defface claude-gravity-agent-nested-bg
  '((((background dark)) :background "#0f2030")
    (((background light)) :background "#e8f0f5"))
  "Background for nested agent sub-branches (2+ levels deep)."
  :group 'claude-gravity)


(defface claude-gravity-agent-margin
  '((((background dark))  :foreground "#5599aa")
    (((background light)) :foreground "#337788"))
  "Fallback face for margin indicator inside agent response cycles.
Content-type faces are preferred; agent distinction is provided
by the background tint (`claude-gravity-agent-bg')."
  :group 'claude-gravity)


(defface claude-gravity-diff-added
  '((((background dark)) :foreground "#88ee88" :background "#1a3a1a")
    (((background light)) :foreground "#006600" :background "#ddffdd"))
  "Face for added text in inline Edit diffs."
  :group 'claude-gravity)


(defface claude-gravity-diff-removed
  '((((background dark)) :foreground "#ee8888" :background "#3a1a1a" :strike-through nil)
    (((background light)) :foreground "#660000" :background "#ffdddd" :strike-through nil))
  "Face for removed text in inline Edit diffs."
  :group 'claude-gravity)


(defface claude-gravity-diff-context
  '((((background dark)) :foreground "#888888")
    (((background light)) :foreground "#666666"))
  "Face for context lines in unified-style Edit diffs."
  :group 'claude-gravity)


(defface claude-gravity-diff-header
  '((((background dark)) :foreground "#7799cc")
    (((background light)) :foreground "#336699"))
  "Face for @@ hunk headers in Edit diffs."
  :group 'claude-gravity)


(defface claude-gravity-plan-margin-added
  '((((background dark)) :foreground "#88ee88")
    (((background light)) :foreground "#22aa22"))
  "Fringe face for added lines in plan revision diff."
  :group 'claude-gravity)


(defface claude-gravity-plan-margin-modified
  '((((background dark)) :foreground "#eeaa44")
    (((background light)) :foreground "#cc8800"))
  "Fringe face for modified lines in plan revision diff."
  :group 'claude-gravity)


(defface claude-gravity-plan-margin-deleted
  '((((background dark)) :foreground "#ee8888")
    (((background light)) :foreground "#cc4444"))
  "Fringe face for deleted-region markers in plan revision diff."
  :group 'claude-gravity)


(defface claude-gravity-phase-boundary
  '((((background dark)) :foreground "#66dd66" :background "#2a2a00")
    (((background light)) :foreground "#228822" :background "#ffffdd"))
  "Face for plan-approved phase boundary prompts."
  :group 'claude-gravity)


(defface claude-gravity-header-title
  '((((background dark))  :weight bold :foreground "white")
    (((background light)) :weight bold :foreground "black"))
  "Face for the main buffer header title."
  :group 'claude-gravity)


(defface claude-gravity-slug
  '((((background dark))  :foreground "dark gray" :slant italic)
    (((background light)) :foreground "gray40" :slant italic))
  "Face for the session slug shown in the header."
  :group 'claude-gravity)

(defface claude-gravity-branch
  '((((class color) (background dark))  :foreground "#5fafaf")
    (((class color) (background light)) :foreground "#2e8b8b")
    (t :foreground "cyan"))
  "Face for git branch name in session list and header."
  :group 'claude-gravity)

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