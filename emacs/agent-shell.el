;;; agent-shell.el --- OpenCode ACP integration -*- lexical-binding: t; -*-

(require 'agent-shell)

;; OpenCode owns provider, model, permission, MCP, and credential selection.
(setq agent-shell-preferred-agent-config 'opencode)

(provide 'dotfiles-agent-shell)
;;; agent-shell.el ends here
