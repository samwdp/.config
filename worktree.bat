@echo off
git worktree list | awk '{print $1}' | fzf
