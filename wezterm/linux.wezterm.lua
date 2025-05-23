-- Pull in the wezterm API
local wezterm = require 'wezterm'
local act = wezterm.action

-- This table will hold the configuration.
local config = {}

-- In newer versions of wezterm, use the config_builder which will
-- help provide clearer error messages
if wezterm.config_builder then
    config = wezterm.config_builder()
end

config.max_fps = 120
config.animation_fps = 120

config.term = "xterm-256color" -- Set the terminal type
config.window_background_opacity = 0.91
config.window_decorations = "NONE | RESIZE"
config.prefer_egl = true

config.default_prog = { 'pwsh', '-NoLogo' }

config.color_scheme = 'GruvboxDarkHard'

config.font = wezterm.font "LiterationMono Nerd Font"

config.disable_default_key_bindings = false
config.window_close_confirmation = "NeverPrompt"

config.tab_bar_at_bottom = true
config.use_fancy_tab_bar = false
config.show_tabs_in_tab_bar = true
config.show_new_tab_button_in_tab_bar = false

config.leader = { key = 'a', mods = 'CTRL', timeout_milliseconds = 1000 }

config.keys = {
    {
        key = "r",
        mods = "LEADER",
        action = wezterm.action_callback(function(window, pane)
            local cmd = [[
                "find ~/projects ~/work -mindepth 1 -maxdepth 3 -type d"
                ]]
            local file = io.popen(cmd)
            local output = file:read("*a")
            file:close()

            local choices = {}
            for directory in string.gmatch(output, "([^\n]+)") do
                table.insert(choices, { label = directory })
            end

            window:perform_action(
                act.InputSelector {
                    title = "Workspaces",
                    choices = choices,
                    action = wezterm.action_callback(function(window, pane, id, label)
                        if label then
                            window:perform_action(act.SwitchToWorkspace {
                                name = label:match("([^/]+)$"),
                                spawn = {
                                    cwd = label,
                                }
                            }, pane)
                        end
                    end),
                    fuzzy = true,
                },
                pane
            )
        end),
    },
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
        key = '\\',
        mods = 'LEADER',
        action = wezterm.action.SplitHorizontal { domain = 'CurrentPaneDomain' },
    },
    {
        key = '-',
        mods = 'LEADER',
        action = wezterm.action.SplitVertical { domain = 'CurrentPaneDomain' },
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
        mods = 'LEADER|SHIFT',
        action = wezterm.action.CloseCurrentTab { confirm = false },
    },
    {
        key = 'q',
        mods = 'LEADER',
        action = wezterm.action.CloseCurrentPane { confirm = false },
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
        key = 'a',
        mods = 'CTRL|SHIFT',
        action = wezterm.action.QuickSelect
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

-- tab bar
config.hide_tab_bar_if_only_one_tab = false
config.tab_bar_at_bottom = true
config.use_fancy_tab_bar = false
config.tab_and_split_indices_are_zero_based = true

-- tmux status
wezterm.on("update-right-status", function(window, _)
    local SOLID_LEFT_ARROW = ""
    local ARROW_FOREGROUND = { Foreground = { Color = "#000000" } }
    local prefix = ""

    if window:leader_is_active() then
        prefix = " " .. utf8.char(0x22ab) -- ocean wave
        SOLID_LEFT_ARROW = utf8.char(0xe0b2)
    end

    if window:active_tab():tab_id() ~= 0 then
        ARROW_FOREGROUND = { Foreground = { Color = "#333" } }
    end -- arrow color based on if tab is first pane

    window:set_left_status(wezterm.format {
        { Background = { Color = "#dca82e" } },
        { Text = prefix },
        ARROW_FOREGROUND,
        { Text = SOLID_LEFT_ARROW }
    })
end)

-- and finally, return the configuration to wezterm
return config
