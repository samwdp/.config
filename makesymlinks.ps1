$wezterm = Resolve-Path "wezterm"
$glaze = Resolve-Path "glazewm"
$powershell = Resolve-Path "PowerShell"
New-Item -Path ../.glzr -ItemType SymbolicLink -Value $glazewm
New-Item -Path ../Documents/PowerShell -ItemType SymbolicLink -Value $powershell
New-Item -Path ../.wezterm.lua -ItemType SymbolicLink -Value $wezterm

