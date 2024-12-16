function Install-Package {
    param (
        [Parameter(Mandatory=$true)]
        [string]$Package,
        [Parameter(Mandatory=$false)]
        [string]$PackageSource,
        [Parameter(Mandatory=$false)]
        [string[]]$ExtraArgs=@()
    )

    switch ($PackageSource) {
        "winget" {
            Write-Output "winget install $($Package) $($ExtraArgs)"

            winget install $Package $ExtraArgs
        }
        Default {
            winget install $Package $ExtraArgs
        }
    }
}

Install-Package "Microsoft.Powershell" "winget"
Install-Package "Git.Git" "winget"
Install-Package "glzr-io.glazewm" "winget"
Install-Package "Nushell.Nushell" "winget"
Install-Package "Starship.Starship" "winget"
Install-Package "wez.wezterm" "winget"
Install-Package "AgileBits.1Password" "winget"
Install-Package "ajeetdsouza.zoxide" "winget"
Install-Package "Microsoft.PowerToys" "winget"
Install-Package "junegunn.fzf" "winget"
Install-Package "rsteube.Carapace" "winget"
Install-Package "Schniz.fmn" "winget"
Install-Package "Google.GoogleDrive" "winget"
Install-Package "Neovim.Neovim.Nightly" "winget" @("--ignore-security-hash","--force")
Install-Package "sharkdp.bat" "winget"
