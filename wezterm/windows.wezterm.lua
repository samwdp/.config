-- Pull in the wezterm API
local wezterm = require 'wezterm'
local act = wezterm.action
local mux = wezterm.mux

-- This table will hold the configuration.
local config = {}

-- In newer versions of wezterm, use the config_builder which will
-- help provide clearer error messages
if wezterm.config_builder then
    config = wezterm.config_builder()
end

-- setting this to false for 1password prompting
--config.mux_enable_ssh_agent = false
config.max_fps = 120
config.animation_fps = 120
config.front_end = "OpenGL"
local gpus = wezterm.gui.enumerate_gpus()

config.webgpu_preferred_adapter = gpus[5]

config.term = "xterm-256color" -- Set the terminal type
config.window_background_opacity = 0
config.win32_system_backdrop = "Tabbed"
-- config.win32_acrylic_accent_color = "#282828"
config.window_decorations = "NONE | RESIZE"

-- config.default_prog = { 'pwsh', '-NoLogo' }
local default_prog = { 'nu' }
config.default_prog = default_prog

config.color_scheme = 'GruvboxDarkHard'

config.font = wezterm.font "LiterationMono Nerd Font"
config.font_size = 14

config.disable_default_key_bindings = false
config.window_close_confirmation = "NeverPrompt"

config.tab_bar_at_bottom = true
config.leader = { key = 'a', mods = 'CTRL', timeout_milliseconds = 1000 }

wezterm.on('gui-startup', function(cmd)
    local args = default_prog
    if cmd then
        args = cmd.args
    end

    local tab, build_pane, window = mux.spawn_window {
        workspace = 'home',
        args = args,
        cwd = wezterm.home_dir .. '/Documents/Obsidian',
    }

    local editor_pane = build_pane:split {
        direction = 'Right',
        size = 0.3,
        cwd = wezterm.home_dir .. "/.config",
    }


    build_pane:send_text 'vi .\r\n'
    editor_pane:send_text 'vi .\r\n'

    mux.set_active_workspace 'home'
end)

-- plugins
local tabline = wezterm.plugin.require("https://github.com/michaelbrusegard/tabline.wez")
tabline.setup({
    options = {
        icons_enabled = true,
        theme = 'GruvboxDarkHard',
        color_overrides = {},
        section_separators = {
            left = wezterm.nerdfonts.ple_right_half_circle_thick,
            right = wezterm.nerdfonts.ple_left_half_circle_thick,
        },
        component_separators = {
            left = wezterm.nerdfonts.ple_right_half_circle_thin,
            right = wezterm.nerdfonts.ple_left_half_circle_thin,
        },
        tab_separators = {
            left = wezterm.nerdfonts.ple_right_half_circle_thick,
            right = wezterm.nerdfonts.ple_left_half_circle_thick,
        },
    },
})
tabline.apply_to_config(config)

local sessionizer = wezterm.plugin.require("https://github.com/mikkasendke/sessionizer.wezterm")
sessionizer.apply_to_config(config)
sessionizer.config = {
    paths = {
        "d:/projects",
        "d:/work",
    },
    command = {
        "fd",
        "-Hs",
        "^.git|worktrees$",
        "-td",
        "--max-depth=3",
        "--prune",
        "--format",
        "{//}",
        "-E node_modules"
    },
    title = "Sessionzer",
    show_default = true,
    show_most_recent = true,
    fuzzy = true,
    additional_directories = {},
    show_additional_before_paths = false,
    description = "Select a workspace: ",
    experimental_branches = false,
}

config.keys = {
    {
        key = "s",
        mods = "LEADER",
        action = sessionizer.show,

        -- action = wezterm.action_callback(function(window, pane)
        --     local cmd = [[ "c:\Users\sam\.config\finddirs.bat" ]]
        --     local file = io.popen(cmd, "r")
        --     local output = file:read("*a")
        --     file:close()
        --
        --     local choices = {}
        --     for directory in string.gmatch(output, "([^\n]+)") do
        --         table.insert(choices, { label = directory })
        --     end
        --
        --     window:perform_action(
        --         act.InputSelector {
        --             title = "Workspaces",
        --             choices = choices,
        --             action = wezterm.action_callback(function(_, _, _, label)
        --                 if label then
        --                     window:perform_action(act.SwitchToWorkspace {
        --                         name = label:match("([^/]+)$"),
        --                         spawn = {
        --                             cwd = label,
        --                         }
        --                     }, pane)
        --                 end
        --             end),
        --             fuzzy = true,
        --         },
        --         pane
        --     )
        -- end),
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
        key = 'h',
        mods = 'LEADER',
        action = wezterm.action { ActivatePaneDirection = "Left" }
    },
    {
        key = 'l',
        mods = 'LEADER',
        action = wezterm.action { ActivatePaneDirection = "Right" }
    },
    {
        key = 'j',
        mods = 'LEADER',
        action = wezterm.action { ActivatePaneDirection = "Down" }
    },
    {
        key = 'k',
        mods = 'LEADER',
        action = wezterm.action { ActivatePaneDirection = "Up" }
    },
    {
        key = 'w',
        mods = 'LEADER',
        action = wezterm.action.ShowLauncherArgs {
            flags = 'FUZZY|WORKSPACES',
        },
    },
}
return config
