Read-Host -Prompt "If this is running in admin, press eny key to continue or CTRL+C to quit" | Out-Null
$wezterm = Resolve-Path "wezterm"
$glaze = Resolve-Path "glazewm"
$powershell = Resolve-Path "Powershell-Profile"
$nvim = Resolve-Path "nvim"
$emacs = Resolve-Path "emacs"
$wezterm = Resolve-Path "wezterm/windows.wezterm.lua"
$nushell = Resolve-Path "nushell"

New-Item -Path ../.glzr -ItemType SymbolicLink -Value $glazewm
New-Item -Path ../Documents/PowerShell -ItemType SymbolicLink -Value $powershell
New-Item -Path ../Documents/WindowsPowerShell -ItemType SymbolicLink -Value $powershell
New-Item -Path ../.wezterm.lua -ItemType SymbolicLink -Value $wezterm
New-Item -Path ..\AppData\Local\nvim -ItemType SymbolicLink -Value $nvim
New-Item -Path ..\AppData\Roamding\.emacs.d -ItemType SymbolicLink -Value $emacs
New-Item -Path ..\AppData\Roamding\nushell -ItemType SymbolicLink -Value $nushell
New-Item -Path wezterm/wezterm.lua -ItemType SymbolicLink -Value $wezterm

