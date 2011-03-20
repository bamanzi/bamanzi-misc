unit UMain;
{$R *.dfm}
{ $R XPTheme.res}
interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, XPMan, Uxtheme, Themes;

type
  TfrmMain = class(TForm)
    OpenDlg: TOpenDialog;
    Panel2: TPanel;
    btOK: TButton;
    btCancel: TButton;
    lblMail: TLabel;
    XPManifest1: TXPManifest;
    Panel3: TGroupBox;
    rgTCInstance: TRadioGroup;
    rgTCPanel: TRadioGroup;
    rgTCTab: TRadioGroup;
    cbOverrideMenuCap: TCheckBox;
    edMenuCaption: TEdit;
    cbTCShellDrive: TCheckBox;
    cbTCShellDir: TCheckBox;
    edTCPath: TEdit;
    lblTCPath: TLabel;
    btSelectTCPath: TButton;
    cbProcessShellLink: TCheckBox;
    cbTCAnyFile: TCheckBox;
    cbSentToLink: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure btCancelClick(Sender: TObject);
    procedure btOKClick(Sender: TObject);
    procedure btSelectTCPathClick(Sender: TObject);
    procedure cbTCShellDirClick(Sender: TObject);
    procedure cbOverrideMenuCapClick(Sender: TObject);
    procedure lblMailMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure lblMailMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure lblMailMouseEnter(Sender: TObject);
    procedure lblMailMouseLeave(Sender: TObject);
    procedure lblMailClick(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    TCPath: string;
    TCFullPath: string;
    TCPathValid: Boolean;
    function GetTCPath: string;
    procedure CheckTCShell;
    procedure TranslateInterface;
    procedure CheckTCPathValid;
  public
  end;

var
  frmMain: TfrmMain;

const
  HCR: HKEY = HKEY_CLASSES_ROOT;
  HCU: HKEY = HKEY_CURRENT_USER;
  HCM: HKEY = HKEY_LOCAL_MACHINE;
  TCEnvironmentVariable: PChar = 'COMMANDER_PATH';
  SendToShellLinkName = 'Open in Total Commander';  

  DirTCShellFull = 'Directory\shell\Open in Total Commander\command';
  DriveTCShellFull = 'Drive\shell\Open in Total Commander\command';
  AnyFileTCShellFull = '*\shell\Open in Total Commander\command';
  LinkTCShellFull = 'lnkfile\shell\Open in Total Commander\command';

  DirTCShell = 'Directory\shell\Open in Total Commander';
  DriveTCShell = 'Drive\shell\Open in Total Commander';
  LinkTCShell = 'lnkfile\shell\Open in Total Commander';
  AnyFileTCShell = '*\shell\Open in Total Commander';

  cDirTCShell = 'Directory\shell\Open in Total Commander';
  cDriveTCShell = 'Drive\shell\Open in Total Commander';
  cLinkTCShell = 'lnkfile\shell\Open in Total Commander';
  cAnyFileTCShell = '*\shell\Open in Total Commander';

  cDefCaption = 'Open in Total Commander';
  cMail = 'mailto:ProgMan13@mail.ru?subject="TC in Shell menu 1.3.5 Modified"';

implementation

uses Registry, ShellAPI, UEngStrings, ULinkProcess;

function TfrmMain.GetTCPath: string;
var
  Path: string;
  Reg: TRegistry;
begin
  Result := '';
  SetLength(Path,300);
  if Windows.GetEnvironmentVariable(TCEnvironmentVariable,PChar(Path),Length(Path)-1) <> 0 then
  begin
    SetLength(Path,StrLen(PChar(Path)));
    Result := Path;
    Exit;
  end;
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HCU;
    if Reg.OpenKeyReadOnly('Software\Ghisler\Total Commander') then
    begin
      if Reg.ValueExists('InstallDir') then Result := Reg.ReadString('InstallDir');
      Exit;
    end;
    Reg.CloseKey;
    Reg.RootKey := HCM;
    if Reg.OpenKeyReadOnly('Software\Ghisler\Total Commander') then
      if Reg.ValueExists('InstallDir') then Result := Reg.ReadString('InstallDir');
  finally
    Reg.Free;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  if SysLocale.DefaultLCID <> 1049 then TranslateInterface;

  Caption := Application.Title;
  Application.HintHidePause := 6000;
  TCPath := GetTCPath;
  edMenuCaption.Text := cDefCaption;
  if TCPath <> '' then TCPath := IncludeTrailingPathDelimiter(TCPath);
  if TCPath <> '' then TCFullPath := TCPath + 'totalcmd.exe';
  edTCPath.Text := TCPath;
  CheckTCPathValid;
end;

procedure TfrmMain.CheckTCShell;
var
  Reg: TRegistry;
  Str: string;
begin
  try
    Str := GetParamsFromLink(SendToShellLinkName);
    if Str <> '' then
    begin
      if AnsiPos('/T',Str) <> 0 then rgTCTab.ItemIndex := 0;
      if AnsiPos('/N',Str) <> 0 then rgTCInstance.ItemIndex := 0;
      if AnsiPos('/L',Str) <> 0 then rgTCPanel.ItemIndex := 0;
      if AnsiPos('/R',Str) <> 0 then rgTCPanel.ItemIndex := 1;
    end;
  except
  end;

  Reg := TRegistry.Create;
  try
    Reg.RootKey := HCR;
    if Reg.OpenKeyReadOnly(DirTCShellFull) then
    begin
      if FileExists(ExtractFilePath(Reg.ReadString(''))+'totalcmd.exe') then
      begin
        cbTCShellDir.Checked := True;
        if AnsiPos('/T',Reg.ReadString('')) <> 0 then rgTCTab.ItemIndex := 0;
        if AnsiPos('/N',Reg.ReadString('')) <> 0 then rgTCInstance.ItemIndex := 0;
        if AnsiPos('/L',Reg.ReadString('')) <> 0 then rgTCPanel.ItemIndex := 0;
        if AnsiPos('/R',Reg.ReadString('')) <> 0 then rgTCPanel.ItemIndex := 1;
        Reg.CloseKey;
        if Reg.OpenKeyReadOnly(cDirTCShell) then
        begin
          if Trim(Reg.ReadString('')) <> '' then
          begin
            cbOverrideMenuCap.Checked := True;
            edMenuCaption.Text := Reg.ReadString('');
          end;
        end;
      end;
      Reg.CloseKey;
    end;

    if Reg.OpenKeyReadOnly(AnyFileTCShellFull) then
    begin
      if FileExists(ExtractFilePath(Reg.ReadString(''))+'totalcmd.exe') then
      begin
        cbTCAnyFile.Checked := True;
        if not cbTCShellDir.Checked then
        begin
          if AnsiPos('/T',Reg.ReadString('')) <> 0 then rgTCTab.ItemIndex := 0;
          if AnsiPos('/N',Reg.ReadString('')) <> 0 then rgTCInstance.ItemIndex := 0;
           if AnsiPos('/L',Reg.ReadString('')) <> 0 then rgTCPanel.ItemIndex := 0;
           if AnsiPos('/R',Reg.ReadString('')) <> 0 then rgTCPanel.ItemIndex := 1;
          Reg.CloseKey;
          if Reg.OpenKeyReadOnly(cAnyFileTCShell) then
          begin
            if Trim(Reg.ReadString('')) <> '' then
            begin
              cbOverrideMenuCap.Checked := True;
              edMenuCaption.Text := Reg.ReadString('');
            end;
          end;
        end;
      end;
      Reg.CloseKey;
    end;

    if Reg.OpenKeyReadOnly(DriveTCShellFull) then
    begin
      if FileExists(ExtractFilePath(Reg.ReadString(''))+'totalcmd.exe') then
      begin
        cbTCShellDrive.Checked := True;
        if not cbTCShellDir.Checked then
        begin
          if AnsiPos('/T',Reg.ReadString('')) <> 0 then rgTCTab.ItemIndex := 0;
          if AnsiPos('/N',Reg.ReadString('')) <> 0 then rgTCInstance.ItemIndex := 0;
          if AnsiPos('/L',Reg.ReadString('')) <> 0 then rgTCPanel.ItemIndex := 0;
          if AnsiPos('/R',Reg.ReadString('')) <> 0 then rgTCPanel.ItemIndex := 1;
          Reg.CloseKey;
          if Reg.OpenKeyReadOnly(cDriveTCShell) then
          begin
            if Trim(Reg.ReadString('')) <> '' then
            begin
              cbOverrideMenuCap.Checked := True;
              edMenuCaption.Text := Reg.ReadString('');
            end;
          end;
        end;
      end;
      Reg.CloseKey;
    end;

    if Reg.OpenKeyReadOnly(LinkTCShellFull) then
    begin
      if AnsiSameText(ExtractFilePath(Reg.ReadString('')) + ExtractFileName(Application.ExeName), Application.ExeName) then
      begin
        cbProcessShellLink.Checked := True;
      end;
    end;
  finally
    Reg.Free;
  end;

  cbSentToLink.Checked := OurLinkExists(SendToShellLinkName);
end;

procedure TfrmMain.btCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.btOKClick(Sender: TObject);
var
  Reg: TRegistry;
  TempStr, LinkArg: string;
begin
  try
    Reg := TRegistry.Create;
    try
      TempStr := '';
      Reg.RootKey := HCR;
      case rgTCInstance.ItemIndex of
        0: TempStr := TempStr + ' /N ';
        1: TempStr := TempStr + ' /O ';
      end; //case
      if rgTCTab.ItemIndex = 0 then TempStr := TempStr + ' /T ';
      case rgTCPanel.ItemIndex of
        0: TempStr := TempStr + ' /L=';
        1: TempStr := TempStr + ' /R=';
      end; //case
      LinkArg := TempStr;
      TempStr := TCFullPath + TempStr + '"%1"';

      if cbTCShellDir.Checked and TCPathValid then
      begin
        if Reg.OpenKey(DirTCShellFull,True) then Reg.WriteString('',TempStr);
        Reg.CloseKey;
        if cbOverrideMenuCap.Checked then
        begin
          if Trim(edMenuCaption.Text) <> '' then
            if Reg.OpenKey(cDirTCShell,True) then Reg.WriteString('',Trim(edMenuCaption.Text));
        end else if Reg.OpenKey(cDirTCShell,True) then Reg.DeleteValue('');
        Reg.CloseKey;
      end;

      if cbTCAnyFile.Checked and TCPathValid then
      begin
        if Reg.OpenKey(AnyFileTCShellFull,True) then Reg.WriteString('',TempStr);
        Reg.CloseKey;
        if cbOverrideMenuCap.Checked then
        begin
          if Trim(edMenuCaption.Text) <> '' then
            if Reg.OpenKey(cAnyFileTCShell,True) then Reg.WriteString('',Trim(edMenuCaption.Text));
        end else if Reg.OpenKey(cAnyFileTCShell,True) then Reg.DeleteValue('');
        Reg.CloseKey;
      end;

      if cbTCShellDrive.Checked and TCPathValid then
      begin
        if Reg.OpenKey(DriveTCShellFull,True) then Reg.WriteString('',TempStr);
        Reg.CloseKey;
        if cbOverrideMenuCap.Checked then
        begin
          if Trim(edMenuCaption.Text) <> '' then
            if Reg.OpenKey(cDriveTCShell,True) then Reg.WriteString('',Trim(edMenuCaption.Text));
        end else if Reg.OpenKey(cDriveTCShell,True) then Reg.DeleteValue('');
        Reg.CloseKey;
      end;
      if cbProcessShellLink.Checked and cbProcessShellLink.Enabled and TCPathValid then
      begin
        TempStr := Application.ExeName + ' "%1"';
        if Reg.OpenKey(LinkTCShellFull,True) then Reg.WriteString('',TempStr);
        Reg.CloseKey;
        if cbOverrideMenuCap.Checked then
        begin
          if Trim(edMenuCaption.Text) <> '' then
            if Reg.OpenKey(cLinkTCShell,True) then Reg.WriteString('',Trim(edMenuCaption.Text));
        end else if Reg.OpenKey(cLinkTCShell,True) then Reg.DeleteValue('');
        Reg.CloseKey;
      end;

      if cbSentToLink.Checked and TCPathValid then
        CreateOurLink(SendToShellLinkName,TCPath + 'totalcmd.exe',LinkArg)
        else DeleteOurLink(SendToShellLinkName);

      if (not cbTCShellDir.Checked) and (cbTCShellDir.State <> cbGrayed) then
      begin
        Reg.DeleteKey(DirTCShell);
      end;
      if (not cbTCAnyFile.Checked) and (cbTCAnyFile.State <> cbGrayed) then
      begin
        Reg.DeleteKey(AnyFileTCShell);
      end;
      if (not cbTCShellDrive.Checked) and (cbTCShellDrive.State <> cbGrayed) then
      begin
        Reg.DeleteKey(DriveTCShell);
      end;
      if (not cbProcessShellLink.Checked) or (not cbProcessShellLink.Enabled) then
      begin
        Reg.DeleteKey(LinkTCShell);
      end;
    finally
      Reg.Free;
    end;

  finally
    Close;
  end;
end;

procedure TfrmMain.btSelectTCPathClick(Sender: TObject);
begin
  OpenDlg.FileName := TCFullPath;
  if OpenDlg.Execute then
  begin
    TCPath := IncludeTrailingPathDelimiter(ExtractFilePath(OpenDlg.FileName));
    TCFullPath := TCPath + 'totalcmd.exe';
    edTCPath.Text := TCPath;
    if not TCPathValid then CheckTCPathValid;
  end;
end;

procedure TfrmMain.cbTCShellDirClick(Sender: TObject);
begin
  rgTCInstance.Enabled := cbTCShellDir.Checked or cbTCShellDrive.Checked
    or cbTCAnyFile.Checked or cbProcessShellLink.Checked or cbSentToLink.Checked;
  rgTCPanel.Enabled := rgTCInstance.Enabled;
  rgTCTab.Enabled := rgTCInstance.Enabled;
  cbOverrideMenuCap.Enabled := cbTCShellDir.Checked or cbTCShellDrive.Checked
    or cbTCAnyFile.Checked or cbProcessShellLink.Checked;
  edMenuCaption.Enabled := cbOverrideMenuCap.Checked and cbOverrideMenuCap.Enabled;
  cbProcessShellLink.Enabled := not cbTCAnyFile.Checked and cbTCShellDir.Checked;
end;

procedure TfrmMain.cbOverrideMenuCapClick(Sender: TObject);
begin
  edMenuCaption.Enabled := cbOverrideMenuCap.Checked;
end;

procedure TfrmMain.lblMailMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button <> mbLeft then Exit;
  TLabel(Sender).Font.Color := clRed;
end;

procedure TfrmMain.lblMailMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button <> mbLeft then Exit;
  TLabel(Sender).Font.Color := clBlue;
end;

procedure TfrmMain.lblMailMouseEnter(Sender: TObject);
begin
  TLabel(Sender).Font.Color := clRed;
  //TLabel(Sender).Font.Style := TLabel(Sender).Font.Style + [fsUnderline];
end;

procedure TfrmMain.lblMailMouseLeave(Sender: TObject);
begin
  TLabel(Sender).Font.Color := clBlue;
  //TLabel(Sender).Font.Style := TLabel(Sender).Font.Style - [fsUnderline];
end;

procedure TfrmMain.lblMailClick(Sender: TObject);
begin
  ShellExecute(Handle,'open',PChar(cMail),nil,nil,SW_SHOW);
  TLabel(Sender).Font.Color := clBlue;
end;

procedure TfrmMain.TranslateInterface;
begin
  lblTCPath.Caption := cEngTCPathLabel;
  btSelectTCPath.Caption := cEngSelectBtn;
  btCancel.Caption := cEngCancelBtn;
  cbTCShellDir.Caption := cEngAddToDir;
  cbTCShellDrive.Caption := cEngAddToDrive;
  cbOverrideMenuCap.Caption := cEngOverrideMenuCap;
  rgTCInstance.Caption := cEngInstance;
  rgTCInstance.Items[0] := cEngInstanceNew;
  rgTCInstance.Items[1] := cEngInstanceCurrent;
  rgTCPanel.Caption := cEngPanel;
  rgTCPanel.Items[0] := cEngPanelLeft;
  rgTCPanel.Items[1] := cEngPanelRight;
  rgTCPanel.Items[2] := cEngPanelDef;
  rgTCTab.Caption := cEngTab;
  rgTCTab.Items[0] := cEngTabNew;
  rgTCTab.Items[1] := cEngTabDef;
  lblMail.Hint := cEngMail;
  cbProcessShellLink.Caption := cEngAddToLink;
  cbTCAnyFile.Caption := cEngTCAnyFile;
  cbSentToLink.Caption := cEngSendToLink;
end;

procedure TfrmMain.CheckTCPathValid;
begin
  TCPathValid := FileExists(TCFullPath);
  if TCPathValid then
  begin
    cbTCShellDir.Checked := False;
    cbTCShellDrive.Checked := False;
    cbTCAnyFile.Checked := False;
    cbProcessShellLink.Checked := False;
  end;
  CheckTCShell;
end;

procedure TfrmMain.FormPaint(Sender: TObject);
var
  TS: TThemeServices;
  P: TThemedTab;
  D: TThemedElementDetails;
begin
  TS := ThemeServices;
  if not TS.ThemesEnabled then Exit;
  P := ttBody;
  D := TS.GetElementDetails(P);
  TS.DrawElement(Canvas.Handle,D,ClientRect);
end;

end.
