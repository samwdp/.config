function Install-Package {
    param (
        [string]$Package,
        [string]$PackageSource
    )

    switch ($PackageSource) {
        "winget" { 
            winget install $Package
        }
        "choco" {
            choco install $Package -y
        }
        Default {}
    }
}


Read-Host -Prompt "If this is running in admin, press eny key to continue or CTRL+C to quit" | Out-Null
Set-ExecutionPolicy Bypass -Scope Process -Force; [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; iex ((New-Object System.Net.WebClient).DownloadString('https://community.chocolatey.org/install.ps1'))

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
Install-Package "CoreyButler.NVMforWindows" "winget"
Install-Package "Google.GoogleDrive" "winget"
Install-Package "neovim" "choco"
