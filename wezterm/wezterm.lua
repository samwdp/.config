-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This table will hold the configuration.
local config = {}

-- In newer versions of wezterm, use the config_builder which will
-- help provide clearer error messages
if wezterm.config_builder then
    config = wezterm.config_builder()
end

config.max_fps = 75
config.animation_fps = 1
config.cursor_blink_ease_in = 'Constant'
config.cursor_blink_ease_out = 'Constant'

config.front_end = "OpenGL"
config.default_cursor_style = "BlinkingBlock"
config.animation_fps = 1
config.cursor_blink_rate = 500
config.term = "xterm-256color" -- Set the terminal type
config.window_background_opacity = 0.91
config.window_decorations = "NONE | RESIZE"
config.prefer_egl = true

config.default_prog = { 'pwsh', '-NoLogo' }

config.color_scheme = 'Gruvbox dark, hard (base16)'
config.font = wezterm.font_with_fallback {
    { family = "Hack Nerd Font",         scale = 1.0 },
    { family = "Symbols Nerd Font Mono", scale = 1.0 },
    { family = "Noto Color Emoji",       scale = 1.0 },
    { family = "Noto Color Emoji",       scale = 1.0 },
    { family = "all-the-icons",          scale = 1.0 },
    { family = "FontAwesome",            scale = 1.0 },
    { family = "Material Icons",         scale = 1.0 },
    { family = "file-icons",             scale = 1.0 },
    { family = "github-octicons",        scale = 1.0 },
    { family = "Weather Icons",          scale = 1.0 },
}

-- config.window_decorations = "RESIZE"
config.disable_default_key_bindings = false
config.window_close_confirmation = "AlwaysPrompt"

config.tab_bar_at_bottom = true
config.use_fancy_tab_bar = false
config.show_tabs_in_tab_bar = true
config.show_new_tab_button_in_tab_bar = false

config.leader = { key = 'a', mods = 'CTRL', timeout_milliseconds = 1000 }

config.keys = {
    -- Send "CTRL-A" to the terminal when pressing CTRL-A, CTRL-A
    {
        key = 'a',
        mods = 'LEADER|CTRL',
        action = wezterm.action.SendKey { key = 'a', mods = 'CTRL' },
    },
    {
        key = 'f',
        mods = 'LEADER',
        action = wezterm.action.ToggleFullScreen
    },
    {
        key = 'p',
        mods = 'CTRL|SHIFT',
        action = wezterm.action.ShowLauncher
    },
    {
        key = 'c',
        mods = 'LEADER',
        action = wezterm.action { SpawnTab = "CurrentPaneDomain" }
    },
    {
        key = 'n',
        mods = 'LEADER',
        action = wezterm.action { ActivateTabRelative = 1 }
    },

    {
        key = 'q',
        mods = 'LEADER',
        action = wezterm.action.CloseCurrentTab { confirm = false },
    },
    {
        key = 'p',
        mods = 'LEADER',
        action = wezterm.action { ActivateTabRelative = -1 }
    },
    {
        key = 'c',
        mods = 'CTRL|SHIFT',
        action = wezterm.action.CopyTo 'Clipboard'
    },
    {
        key = 'v',
        mods = 'CTRL|SHIFT',
        action = wezterm.action.PasteFrom 'Clipboard'
    },
    {
        key = 'n',
        mods = 'CTRL|SHIFT',
        action = wezterm.action.PromptInputLine {
            description = wezterm.format {
                { Attribute = { Intensity = 'Bold' } },
                { Foreground = { AnsiColor = 'Fuchsia' } },
                { Text = 'Enter name for new workspace' },
            },
            action = wezterm.action_callback(function(window, pane, line)
                -- line will be `nil` if they hit escape without entering anything
                -- An empty string if they just hit enter
                -- Or the actual line of text they wrote
                if line then
                    window:perform_action(
                        wezterm.action.SwitchToWorkspace {
                            name = line,
                        },
                        pane
                    )
                end
            end),
        },
    },
    {
        key = 'w',
        mods = 'LEADER',
        action = wezterm.action.ShowLauncherArgs {
            flags = 'FUZZY|WORKSPACES',
        },
    },
}

-- and finally, return the configuration to wezterm
return config
