# Agent Guide

## Workflow
- Read relevant files before proposing changes.
- Share a short implementation approach first.
- Wait for explicit confirmation before making code edits.

## Scope
- Focus only on the requested task.
- Do not add features or refactor unrelated code.
- If requirements are unclear, ask before implementing.

## Code Style
- Prefer clean, readable code.
- Do not add obvious inline comments.
- Keep existing comments unless asked to remove them.
- Add comments only when logic is genuinely hard to follow.

## Safety
- Never revert or overwrite unrelated local changes.
- Avoid destructive git/file operations unless explicitly requested.
- If unexpected changes appear while working, stop and ask.

## Tooling
- Use `rg`/`rg --files` for fast search.
- Keep commands and edits minimal and targeted.
- Prefer small, reviewable diffs.

## Validation
- Run relevant checks for the changed area when possible.
- Report what was changed and which checks were run.
- If checks were not run, state that explicitly.

## Emacs Locations
- Repo-managed Doom config: `.doom.d/`
- Machine-local overrides: `~/.emacs.local`
- Emacs runtime/state and possible extra config: `~/.emacs.d/`
- Prefer editing `.doom.d/` for this repo; only edit paths outside the repo when explicitly requested.
