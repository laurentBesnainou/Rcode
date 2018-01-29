#define MyAppName "myapp"
#define MyAppVersion "0.0.0"
#define MyAppExeName "myapp.bat"
#define RVersion "3.3.2"
#define IncludeR false
#define MyAppPublisher ""
#define MyAppURL ""


[Setup]
AppId = {{CXQTRX1F-S2K7-C111-501K-XOLNDYJS2VVT}
AppName = {#MyAppName}
DefaultDirName = {userdocs}\{#MyAppName}
DefaultGroupName = {#MyAppName}
OutputDir = RInno_installer
OutputBaseFilename = setup_{#MyAppName}
SetupIconFile = setup.ico
AppVersion = {#MyAppVersion}
AppPublisher = {#MyAppPublisher}
AppPublisherURL = {#MyAppURL}
AppSupportURL = {#MyAppURL}
AppUpdatesURL = {#MyAppURL}
PrivilegesRequired = lowest
InfoBeforeFile = infobefore.txt
Compression = lzma2/ultra64
SolidCompression = yes

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"

[Files]
Source: "{#MyAppExeName}"; DestDir: "{app}"; Flags: ignoreversion
#if IncludeR
    Source: "R-{#RVersion}-win.exe"; DestDir: "{tmp}"; Check: RNeeded
#endif
Source: "default.ico"; DestDir: "{app}"; Flags: ignoreversion;
Source: "myapp.bat"; DestDir: "{app}"; Flags: ignoreversion;
Source: "server.R"; DestDir: "{app}"; Flags: ignoreversion;
Source: "setup.ico"; DestDir: "{app}"; Flags: ignoreversion;
Source: "ui.R"; DestDir: "{app}"; Flags: ignoreversion;
Source: "utils\app.R"; DestDir: "{app}\utils"; Flags: ignoreversion;
Source: "utils\config.cfg"; DestDir: "{app}\utils"; Flags: ignoreversion;
Source: "utils\ensure.R"; DestDir: "{app}\utils"; Flags: ignoreversion;
Source: "utils\get_app_from_app_url.R"; DestDir: "{app}\utils"; Flags: ignoreversion;
Source: "utils\package_manager.R"; DestDir: "{app}\utils"; Flags: ignoreversion;
Source: "utils\wsf\js\JSON.minify.js"; DestDir: "{app}\utils\wsf\js"; Flags: ignoreversion;
Source: "utils\wsf\js\json2.js"; DestDir: "{app}\utils\wsf\js"; Flags: ignoreversion;
Source: "utils\wsf\js\run.js"; DestDir: "{app}\utils\wsf\js"; Flags: ignoreversion;
Source: "utils\wsf\run.wsf"; DestDir: "{app}\utils\wsf"; Flags: ignoreversion;

[Icons]
Name: "{group}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; IconFilename: "{app}\default.ico"
Name: "{group}\{cm:UninstallProgram,{#MyAppName}}"; Filename: "{uninstallexe}"
Name: "{commonprograms}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; IconFilename: "{app}\default.ico"
Name: "{commondesktop}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Tasks: desktopicon; IconFilename: "{app}\default.ico"

[Run]
#if IncludeR
	Filename: "{tmp}\R-{#RVersion}-win.exe"; Parameters: "/SILENT"; WorkingDir: {tmp}; Flags: skipifdoesntexist; StatusMsg: "Installing R if needed"
#endif
Filename: "{app}\{#MyAppExeName}"; Description: "{cm:LaunchProgram,{#StringChange(MyAppName, '&', '&&')}}"; Flags: shellexec postinstall skipifsilent


[Code]
// Is R installed?
function RDetected(): boolean;
var
    success: boolean;
begin
  success := RegKeyExists(HKLM, 'Software\R-Core\R\{#RVersion}');
  begin
    Result := success;
  end;
end;

// If R is not detected, it is needed
function RNeeded(): Boolean;
begin
  Result := (RDetected = false);
end;


