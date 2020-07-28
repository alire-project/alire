// Run with:
// ./alire-x86_64-*.exe --platform minimal --script auto-install.js -v
// To specify the installation directory, add InstallPrefix="C:\custom_install\path\"
// To see graphically what's happening, remove "--platform minimal"

var install_dir = installer.value("InstallPrefix")

function Controller()
{
    installer.setDefaultPageVisible(QInstaller.Introduction, true);

    var page = gui.pageWidgetByObjectName( "ComponentSelectionPage" );
    page.selectComponent( "com.msys2.root" );
    page.selectComponent( "com.msys2.root.base" );

    installer.setDefaultPageVisible(QInstaller.ComponentSelection, false);
    installer.setDefaultPageVisible(QInstaller.StartMenuSelection, false);
    installer.setDefaultPageVisible(QInstaller.LicenseCheck, false);
    installer.setDefaultPageVisible(QInstaller.ReadyForInstallation, false);
    installer.setDefaultPageVisible(QInstaller.PerformInstallation, true);
    installer.setDefaultPageVisible(QInstaller.InstallationFinished, true);
    installer.setDefaultPageVisible(QInstaller.FinishedPage, true);
    ComponentSelectionPage.selectAll();
    installer.autoRejectMessageBoxes();
    var result = QMessageBox.question("quit.question", "Installer", "Do you want to quit the installer?",
                                      QMessageBox.Yes | QMessageBox.No);

    console.log("(console.log) result " + result);

    installer.setMessageBoxAutomaticAnswer("OverwriteTargetDirectory", QMessageBox.Yes);
    installer.setMessageBoxAutomaticAnswer("stopProcessesForUpdates", QMessageBox.Ignore);
}

Controller.prototype.IntroductionPageCallback = function()
{
    gui.clickButton(buttons.NextButton);
}

Controller.prototype.TargetDirectoryPageCallback = function()
{
    gui.currentPageWidget().TargetDirectoryLineEdit.setText(install_dir);
    gui.clickButton(buttons.NextButton);
}

Controller.prototype.StartMenuDirectoryPageCallback = function()
{
    gui.clickButton(buttons.NextButton);
}

Controller.prototype.PerformInstallationPageCallback = function()
{
    var page = gui.pageWidgetByObjectName("PerformInstallationPage");
    installer.setAutomatedPageSwitchEnabled(true);
    gui.clickButton(buttons.NextButton);
}

Controller.prototype.InstallationFinishedPageCallback = function()
{
    console.log("(console.log) InstallationFinishedPageCallback ");
    var checkBox = gui.pageWidgetByObjectName("RunItCheckBox");
    console.log("typeof checkBox is " + typeof checkBox);
    assert(typeof checkBox === 'object');

    var page = gui.pageWidgetByObjectName("InstallationFinishedPage");
    gui.clickButton(buttons.NexthButton);
}

Controller.prototype.FinishedPageCallback = function()
{
    console.log("(console.log) FinishedPageCallback ");
//    var checkBox = gui.pageWidgetByObjectName("RunItCheckBox");
//    console.log("typeof checkBox is " + typeof checkBox);
//    assert(typeof checkBox === 'object');

    var page = gui.pageWidgetByObjectName("FinishedPage");
    page.RunItCheckBox.checked = false;
    gui.clickButton(buttons.FinishButton);
}

/*
Controller.prototype.ComponentSelectionPageCallback = function()
{
    var page = gui.pageWidgetByObjectName( "ComponentSelectionPage" )
    page.deselectComponent( "com.nokia.ndk.tools.maemo.usbdriver" )
    gui.clickButton( buttons.NextButton )
}
*/
