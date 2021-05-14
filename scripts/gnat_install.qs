function Controller() {
    installer.setValue("TargetDir", installer.value("InstallPrefix"));
    installer.autoRejectMessageBoxes();
    installer.installationFinished.connect(function() {
        gui.clickButton(buttons.NextButton);
    })
}

Controller.prototype.ComponentSelectionPageCallback = function()
{
    var page = gui.currentPageWidget();

    page.deselectAll();
    page.selectComponent("com.adacore.gnat");
    gui.clickButton(buttons.NextButton);
}

Controller.prototype.LicenseAgreementPageCallback = function()
{
    var page = gui.pageById(QInstaller.LicenseCheck);

    page.AcceptLicenseRadioButton.click();
    gui.clickButton(buttons.NextButton);
}

Controller.prototype.IntroductionPageCallback =
Controller.prototype.TargetDirectoryPageCallback =
Controller.prototype.ReadyForInstallationPageCallback = function()
{
    gui.clickButton(buttons.NextButton);
}

Controller.prototype.FinishedPageCallback = function()
{
    gui.clickButton(buttons.FinishButton);
}
