# Herdr Annotate Design

## Goal

Install the official Full variant of `plannotator/herdr-annotate`, make every
official Full action reachable through Herdr keybindings, and document the
required Herdr plugin in this repository.

## Scope

- Install `plannotator/herdr-annotate` with Herdr's plugin manager.
- Add the eight documented Full-variant `plugin_action` bindings to
  `.config/herdr/config.toml`.
- Add a README section that lists the required plugin, its installation
  command, and the configured keybindings.

The existing custom pane-management and workspace-picker bindings remain
unchanged.

## Keybindings

| Key | Plugin action | Purpose |
| --- | --- | --- |
| `prefix+a` | `annotate.capture` | Annotate selected terminal text |
| `prefix+shift+a` | `annotate.copy-context` | Copy annotations as context |
| `prefix+ctrl+a` | `annotate.copy-archive` | Copy and archive annotations |
| `prefix+ctrl+v` | `annotate.paste-archive` | Paste and archive annotations into the agent prompt |
| `prefix+ctrl+s` | `annotate.send-archive` | Send and archive annotations to the agent |
| `prefix+m` | `annotate.manage` | Manage annotations |
| `prefix+o` | `annotate.open` | Review documents in the current folder |
| `prefix+shift+o` | `annotate.last` | Review the agent's last reply |

`prefix+ctrl+o` is also included for `annotate.last-newest`, which opens the
newest agent reply directly. It does not conflict with an enabled binding in
the existing configuration.

## Validation

1. Run `herdr plugin list --plugin annotate --json` to verify that the Full
   plugin is installed and enabled.
2. Run `herdr config check` to validate TOML syntax and plugin action names.
3. Run `herdr server reload-config` so the running server uses the new
   bindings.
4. Inspect the targeted diff to ensure existing uncommitted configuration is
   preserved and README documentation matches the configuration.
