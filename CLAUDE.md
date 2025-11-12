# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is a personal dotfiles repository for macOS development environments, managing configuration files, development tools, and system setup automation. The repository uses symlinks to manage dotfiles and includes comprehensive tool management through Homebrew, aqua, and mise.

## Installation and Setup Commands

### Initial Setup

```bash
# Clone the repository
git clone https://github.com/tkmpypy/dotfiles.git ~/ghq/github.com/tkmpypy/dotfiles
cd ~/ghq/github.com/tkmpypy/dotfiles

# Run the complete installation
./scripts/install.sh
```

### Manual Tool Installation

```bash
# Install Homebrew packages
cd scripts/Darwin && brew bundle --file Brewfile

# Install aqua-managed CLI tools
aqua install

# Install mise-managed language runtimes
mise install
```

## Architecture and Structure

### Core Components

1. **Installation System (`scripts/install.sh`)**
   - OS-agnostic entry point that detects macOS and runs Darwin-specific scripts
   - Creates symlinks for dotfiles (`.zshrc`, `.vimrc`, `.tmux.conf`, etc.)
   - Links config directories to `~/.config/`
   - Runs OS-specific setup scripts in sequence

2. **macOS Setup (`scripts/Darwin/`)**
   - `root.sh`: Orchestrates clone.sh, system.sh, and brew.sh
   - `clone.sh`: Installs tmux plugin manager
   - `system.sh`: macOS system configuration
   - `brew.sh`: Homebrew installation and package management
   - `Brewfile`: Declarative package list for Homebrew

3. **Tool Management Strategy**
   - **Homebrew** (`scripts/Darwin/Brewfile`): System packages, GUI applications, fonts
   - **aqua** (`.config/aqua/aqua.yaml`): CLI development tools with version pinning
   - **mise** (`.config/mise/config.toml`): Programming language runtimes and versions

### Configuration Organization

```
.config/
├── git/           # Git configuration and global gitignore
├── nvim/          # Neovim configuration with Lua plugins
├── zsh/           # Zsh shell configuration modules
├── fish/          # Fish shell configuration
├── wezterm/       # Terminal emulator configuration
├── tmux/          # Terminal multiplexer settings
├── helix/         # Helix editor configuration
├── lazygit/       # Git TUI configuration
└── aqua/          # CLI tool version management
```

### Custom Utilities (`bin/`)

- `tmux-switch-client-from-ghq`: Integrates ghq repository management with tmux sessions
- `tmux-switch-client-with-new-session`: Tmux session management helper
- `fzf-gcloud-config-set-project`: GCloud project switcher with fzf
- `bwcp`: Bitwarden clipboard utility
- `denvmod`: Development environment modifier

## Development Workflow

### Making Changes to Dotfiles

Since files are symlinked, editing them directly in the home directory will modify the repository:

```bash
# Edit configurations directly
nvim ~/.config/fish/config.fish # Modifies the symlinked file in the repo

# Or edit in the repository
nvim ~/ghq/github.com/tkmpypy/dotfiles/.config/fish/config.fish
```

### Adding New Tools

1. **For GUI apps or system packages**: Add to `scripts/Darwin/Brewfile`
2. **For CLI tools**: Add to `.config/aqua/aqua.yaml` with version pinning
3. **For programming languages**: Add to `.config/mise/config.toml`

### Tool Version Management

- **aqua**: Provides reproducible CLI tool versions with automatic updates via Renovate
- **mise**: Manages programming language runtimes (Go, Node.js, Python, etc.)
- Both tools support project-specific version overrides via local config files

## Dependency Management

- **Renovate** (`renovate.json`): Automated dependency updates for aqua tools
- **Homebrew Bundle**: Lock file tracking (`scripts/Darwin/Brewfile.lock.json`)
- **Version pinning**: All tools in aqua.yaml have specific versions for reproducibility

## Key Integrations

- **ghq + tmux**: Repository management integrated with terminal sessions
- **fzf**: Fuzzy finding throughout shell and tool configurations
- **Neovim**: Extensive Lua-based configuration with custom local plugins
- **Shell agnostic**: Supports both zsh and fish shells with shared tool configurations

