# This script dispatches to the Ada patcher, after building it.

# Set strict mode for PowerShell to exit on error
$ErrorActionPreference = "Stop"

$bin = "support/version_patcher/bin/version_patcher.exe"

# If the binary is already in place, do nothing
if (Test-Path $bin) {
    Write-Output "Patcher already built."
} elseif (Get-Command gprbuild -ErrorAction SilentlyContinue) {
    Write-Output "Building patcher with gprbuild..."
    gprbuild -P support/version_patcher/version_patcher.gpr
} elseif (Get-Command alr -ErrorAction SilentlyContinue) {
    Write-Output "Building patcher with alr..."
    alr -C (Split-Path $bin) build
} else {
    Write-Output "WARNING: No Ada tool available to build patcher, skipping."
    exit 0
}

& $bin @args

Write-Output "Resulting version file:"
Get-Content src/alire/alire-version.ads | Select-String "Current_Str"
