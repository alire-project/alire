function Component() {
    var targetDir = installer.value("TargetDir", "@ApplicationsDir@/Alire")

    installer.setValue("TargetDir", targetDir);
    installer.setDefaultPageVisible(QInstaller.Introduction, false);
    installer.setDefaultPageVisible(QInstaller.TargetDirectory, true);
    installer.setDefaultPageVisible(QInstaller.ComponentSelection, false);
    installer.setDefaultPageVisible(QInstaller.ReadyForInstallation, false);
    installer.setDefaultPageVisible(QInstaller.StartMenuSelection, true);
    installer.setDefaultPageVisible(QInstaller.LicenseCheck, false);
}

Component.prototype.isDefault = function()
{
    // select the component by default
    return true;
}

function createShortcuts()
{
	var shortcuts = ["@StartMenuDir@/Alire.lnk", "@DesktopDir@/Alire.lnk"];
	for (shortcut of shortcuts) {
		component.addOperation("CreateShortcut", "powershell", shortcut,
                               "-NoExit -Command \"$env:Path += \"\"\";@TargetDir@\\bin\"\"\"; \"$host.UI.RawUI.BackgroundColor = \"\"\"DarkBlue\"\"\"; clear-host\"",
                               "workingDirectory=@HomeDir@",
	                           "iconPath=@TargetDir@/share/alire/alr_icon.ico",
	                           "description=Start App");

    }
}

Component.prototype.createOperations = function()
{
    component.createOperations();
    createShortcuts();
}
