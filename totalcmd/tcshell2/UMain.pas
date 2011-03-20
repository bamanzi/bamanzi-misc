{
TODO:
  * Archive associations
    * Customize format description
    * UI: use different color to show formats already associated
  * Support TC launcher (not only totalcmd.exe)
  * Make command line could be edited manually

* 1.6.0
  * Now you can use `TCShell2 /path/to/filename' to locate file in TC

* 1.5.7
  * Archive associations:
    + Now you can double click to set the description for an extension
    + Support jar/xpi/r00/r01 as TC's built-in archive
    + Refresh archive associations after Apply clicked
    - Fixed a bug when restoring old associations
  + Support opening in source/target panel ( /S )

* 1.5.6 (2008-10-31)
  - Fixed some bugs

* 1.5.5
  + Added an edit box to show the command line

* 1.5.4

* 1.5.3
  * Now based on ProgMan13's 1.3.6 (not released on totalcmd.net), which
     + Added support for different wincmd.ini
     + Fixed bug while parsing command line
     + Guess location of wincmd.ini (COMMANDER_INI, then registry, then totalcmd.exe's path)
  + UI layout changed, Path for totalcmd.exe and wincmd.ini now above the tabs

* 1.5.2  (2007-7.11)
  + Added support for RedirectSection (see wincmd.hlp for detail info)
  + Experimental: register TC as default action for directory (shell folder not supported,
        such My Computer)

* 1.5.1
  + Added archive association feature
  + UI layout changed, now we have three tabs

}
unit UMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, XPMan, StdCtrls, ExtCtrls, ComCtrls;

type
  TfrmMain = class(TForm)
    Panel2: TPanel;
    btOK: TButton;
    btCancel: TButton;
    btnApply: TButton;
    pgc1: TPageControl;
    ts1: TTabSheet;
    cbTCShellDir: TCheckBox;
    cbTCShellDrive: TCheckBox;
    cbProcessShellLink: TCheckBox;
    cbTCAnyFile: TCheckBox;
    cbOverrideMenuCap: TCheckBox;
    edMenuCaption: TEdit;
    cbSentToLink: TCheckBox;
    cbTCShellDirDefault: TCheckBox;
    ts2: TTabSheet;
    Label8: TLabel;
    lvArchiveExts: TListView;
    ts3: TTabSheet;
    rgTCInstance: TRadioGroup;
    rgTCPanel: TRadioGroup;
    rgTCTab: TRadioGroup;
    OpenDlg: TOpenDialog;
    XPManifest1: TXPManifest;
    lblMail: TLabel;
    Label1: TLabel;
    edtCmdLine: TEdit;
    lblAuthor2: TLabel;
    lblMain2: TLabel;
    lblTCPath: TLabel;
    edTCPath: TEdit;
    btSelectTCPath: TButton;
    cbINI: TCheckBox;
    edINI: TEdit;
    btINI: TButton;
    lblShellLinkHint: TLabel;
    procedure btnApplyClick(Sender: TObject);
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
    procedure rgTCInstanceClick(Sender: TObject);
    procedure btINIClick(Sender: TObject);
    procedure cbINIClick(Sender: TObject);
    procedure edINIChange(Sender: TObject);
    procedure lvArchiveExtsDblClick(Sender: TObject);
  private
    TCPath: string;
    TCFullPath: string;
    TCPathValid: Boolean;
    function GetTCPath: string;
    procedure CheckTCShell;
    procedure CheckTCPackers;
    procedure GuessDefaultIniPath;    
    function GenerateCmdLineOptions: String;
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

  DirTCShellFull = 'Directory\shell\wincmd\command';
  DriveTCShellFull = 'Drive\shell\Open in Total Commander\command';
  AnyFileTCShellFull = '*\shell\Open in Total Commander\command';
  LinkTCShellFull = 'lnkfile\shell\Open in Total Commander\command';

  DirTCShell = 'Directory\shell\wincmd';
  DriveTCShell = 'Drive\shell\Open in Total Commander';
  LinkTCShell = 'lnkfile\shell\Open in Total Commander';
  AnyFileTCShell = '*\shell\Open in Total Commander';

  cDirTCShell = 'Directory\shell\wincmd';
  cDriveTCShell = 'Drive\shell\Open in Total Commander';
  cLinkTCShell = 'lnkfile\shell\Open in Total Commander';
  cAnyFileTCShell = '*\shell\Open in Total Commander';

  cDialogTCFileNameFilter = 'totalcmd.exe|totalcmd.exe';
  cDialogINIFileNameFilter = 'wincmd.ini, totalcmd.ini|wincmd.ini;totalcmd.ini|*.ini|*.ini';
  
  cDefCaption = 'Open in Total Commander';
//  cMail = 'mailto:ProgMan13@mail.ru?subject="TC in Shell menu 1.3.5 Modified"';

implementation

{$R *.dfm}
{$R XPTheme.res}

uses Registry, ShellAPI, IniFiles, Themes, UEngStrings, ULinkProcess;

const
  COL_PACKER = 0;
  COL_DESC = 1;
  COL_ASSOCIATION = 2;

function ExtractWordEx(N: Integer; const S: string; const WordDelim: Char): string;
var
  Counter, WordCount: Integer;
  Char: string;
begin
  Result := '';
  if S = '' then Exit;
  Counter := 0;
  WordCount := 0;
  while (Counter < Length(S)) do
  begin
    Inc(Counter);
    Char := S[Counter];
    if Char = WordDelim then
    begin
      Inc(WordCount);
      if WordCount = N then Break else
      begin
        Result := '';
        Continue;
      end;
    end;
    Result := Result + Char;
  end;
end;

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

  pgc1.ActivePageIndex := 0;
  Caption := Application.Title;
  Application.HintHidePause := 6000;

  TCPath := GetTCPath;
  edMenuCaption.Text := cDefCaption;
  if TCPath <> '' then TCPath := IncludeTrailingPathDelimiter(TCPath);
  if TCPath <> '' then TCFullPath := TCPath + 'totalcmd.exe';

  edTCPath.Text := TCPath;
  GuessDefaultIniPath;
  
  CheckTCPathValid;
end;

procedure TfrmMain.CheckTCShell;
  procedure ParseParamsToUI(params: string);
  var
    Position: Integer;
    IniPath: string;
  begin
      if AnsiPos('/T',params) <> 0 then rgTCTab.ItemIndex := 0;
      if AnsiPos('/N',params) <> 0 then rgTCInstance.ItemIndex := 0;
      if AnsiPos('/S', params)<> 0 then
      begin
         if AnsiPos('/L',params) <> 0 then rgTCPanel.ItemIndex := 3;  //source panel
         if AnsiPos('/R',params) <> 0 then rgTCPanel.ItemIndex := 4;  //target panel
      end
      else
      begin
         if AnsiPos('/L',params) <> 0 then rgTCPanel.ItemIndex := 0;  //left panel
         if AnsiPos('/R',params) <> 0 then rgTCPanel.ItemIndex := 1;  //right panel
      end;

      if AnsiPos('/i=',params) <> 0 then
      begin
        cbINI.Checked := True;
        Position := AnsiPos('/i=',params);
        INIPath := AnsiDequotedStr(Trim(ExtractWordEx(1,Copy(params,Position + 3,MaxInt),'/')),'"');
        edINI.Text := INIPath;
      end;
  end;
var
  Reg: TRegistry;
  str: string;

begin
  try
    str := GetParamsFromLink(SendToShellLinkName);
    if Str <> '' then
    begin
      ParseParamsToUI(str);
    end;
  except
  end;

  Reg := TRegistry.Create;
  try
    Reg.RootKey := HCR;
    if Reg.OpenKeyReadOnly(DirTCShellFull) then
    begin
      str := Reg.ReadString('');
      if FileExists(ExtractFilePath(str)+'totalcmd.exe') then
      begin
        edtCmdLine.Text := str;

        cbTCShellDir.Checked := True;

        ParseParamsToUI(str);

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

      //check default action
      if Reg.OpenKey('Directory\shell', false) then
      begin
        cbTCShellDirDefault.Checked := 'wincmd'=Reg.ReadString('');
        Reg.CloseKey;
      end;
      
    end;

    if Reg.OpenKeyReadOnly(AnyFileTCShellFull) then
    begin
      if FileExists(ExtractFilePath(Reg.ReadString(''))+'totalcmd.exe') then
      begin
        cbTCAnyFile.Checked := True;
        if not cbTCShellDir.Checked then
        begin
          ParseParamsToUI(Reg.ReadString(''));

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
          ParseParamsToUI(Reg.ReadString(''));
                    
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
      if AnsiSameText(
          ExtractFilePath(ExtractWordEx(1, Reg.ReadString(''), '/'))
          + ExtractFileName(Application.ExeName),
          Application.ExeName) then
      begin
        cbProcessShellLink.Checked := True;
      end;
      Reg.CloseKey;
    end;

  finally
    Reg.Free;
  end;

  cbSentToLink.Checked := OurLinkExists(SendToShellLinkName);
end;

procedure TfrmMain.CheckTCPackers;
  function GetTCPackerPluginExts(extlist: TStrings): integer;
  var
    wcmdini: String;
    ini: TIniFile;
    //i: integer;
    exts: TStrings;
  begin
    Result := 0;
    wcmdini := edINI.Text;
    if (Copy(wcmdini, 1, 2)='.\') then
      wcmdini := TCPath + edINI.Text;
    //edINI.Hint := wcmdini;      

    if not FileExists(wcmdini) then
    begin
      lvArchiveExts.Hint := 'wincmd.ini not found. You''better launch this tool from inside TC.';
      lvArchiveExts.Enabled := False;
      exit;
    end
    else
      lvArchiveExts.Enabled := True;

      ini := TIniFile.Create(wcmdini);
      exts := TStringList.Create;
      try
        if ''<> ini.ReadString('PackerPlugins', 'RedirectSection', '') then
        begin
          wcmdini := ini.ReadString('PackerPlugins', 'RedirectSection', '');
          if Pos(wcmdini, ':')=0 then //not full path
            wcmdini := TCPath + wcmdini;
          if FileExists(wcmdini) then
          begin
             ini.Free;
             ini := TIniFile.Create(wcmdini);
          end;
        end;
        
        ini.ReadSectionValues('PackerPlugins', exts);
//        for i := 0  to exts.Count-1 do
//        begin
//          extlist.Add(exts.Names[i]);
//        end;
        extlist.AddStrings(exts);
        Result := exts.Count;
      finally
        exts.Free;
        ini.Free;
      end;

  end;
var
  Reg: TRegistry;
  ext, unpacker, typename: string;
  exts : TStrings;
  item: TListItem;
  i: integer;
begin
    //now process archives
    exts := TStringList.Create;
    exts.Add('zip=(built-in)');
    exts.Add('rar=(built-in)');
    exts.Add('tar=(built-in)');
    exts.Add('arj=(built-in)');
    exts.Add('uc2=(built-in)');
    exts.Add('gz=(built-in)');
    exts.Add('lha=(built-in)');
    exts.Add('ace=(built-in)');
    exts.Add('tgz=(built-in)');

    exts.Add('jar=(built-in)');
    exts.Add('xpi=(built-in)');
    exts.Add('r00=(built-in)');
    exts.Add('r01=(built-in)');

    GetTCPackerPluginExts(exts);

  Reg := TRegistry.Create;
  try
    Reg.RootKey := HCR;

      lvArchiveExts.Items.Clear;
//      clbArchiveExts.Items.AddStrings(exts);
      for i := 0 to exts.Count-1 do
      begin
        ext := exts.Names[i];
        unpacker := ExtractFileName(exts.ValueFromIndex[i]);

        item := lvArchiveExts.Items.Add;
        item.Caption := ext;
        item.SubItems.Add(unpacker);
        item.SubItems.Add('');   //description
        item.SubItems.Add('');   //association

        //e.g. open 'HKCR\.rar'
        if Reg.OpenKeyReadOnly('.'+ext) then
        begin
          typename := Reg.ReadString('');   //e.g.
          Reg.CloseKey; //close

          if ''=typename then
            typename := '.'+ ext;  //defaults to '.ext'

          //type description
          if Reg.OpenKeyReadOnly(typename) then
          begin
            item.SubItems[COL_DESC] := Reg.ReadString('');
            Reg.CloseKey;
          end
          else
            item.SubItems.Add('');

          if Reg.OpenKeyReadOnly(typename + '\shell\open\command') then
          begin
            item.SubItems[COL_ASSOCIATION] := Reg.ReadString('');
//            OutputDebugString( PAnsiChar(ExtractFilePath(Reg.ReadString(''))));
            if AnsiSameText(ExtractFilePath(ExtractWordEx(1, Reg.ReadString(''), '/')) + 'totalcmd.exe', TCFullPath) then
            begin
              item.Checked := True;
              item.Data := Pointer(1);  //mark this one already aossciated
            end;
            Reg.CloseKey;
          end;
        end;
      end;
   finally
      Reg.Free;
   end;
end;

procedure TfrmMain.btCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.btOKClick(Sender: TObject);
begin
    btnApplyClick(btnApply);
    self.Close;
end;

function TfrmMain.GenerateCmdLineOptions: String;
var
    tempstr : string;
    i: integer;
begin
    tempstr := '';

      if cbINI.Checked then
      begin
        edINI.Text := Trim(edINI.Text);
        TempStr := edINI.Text;
        for I := 1 to Length(TempStr) do
          if TempStr[I] = '/' then TempStr[I] := '\';
        edINI.Text := TempStr;
        if edINI.Text <> '' then TempStr := ' /i="' + edINI.Text + '"';
      end;

    case rgTCInstance.ItemIndex of
      0: tempstr := tempstr + ' /N ';
      1: tempstr := tempstr + ' /O ';
    end; //case
    if rgTCTab.ItemIndex = 0 then tempstr := tempstr + ' /T ';
    case rgTCPanel.ItemIndex of
      0: tempstr := tempstr + ' /L=';
      1: tempstr := tempstr + ' /R=';
      3: tempstr := tempstr + ' /S /L=';
      4: tempstr := tempstr + ' /S /R=';
    end; //case


    Result := tempstr;
    edtCmdLine.Text := TCFullPath + tempstr + '"%1"';
end;

procedure TfrmMain.btnApplyClick(Sender: TObject);
var
  Reg: TRegistry;
  opencmd, tempstr, linkarg: string;
  i: integer;
  ext, typename: string;
begin
    Reg := TRegistry.Create;
    try
      Reg.RootKey := HCR;
      tempstr := GenerateCmdLineOptions;

      linkarg := tempstr;
      opencmd := TCFullPath + tempstr + '"%1"';

      if cbTCShellDir.Checked and TCPathValid then
      begin
        if Reg.OpenKey(DirTCShellFull,True) then Reg.WriteString('',opencmd);
        Reg.CloseKey;
        //if cbOverrideMenuCap.Checked then
        //begin
        //  if Trim(edMenuCaption.Text) <> '' then
            if Reg.OpenKey(cDirTCShell,True) then Reg.WriteString('',Trim(edMenuCaption.Text));
        //end else if Reg.OpenKey(cDirTCShell,True) then Reg.DeleteValue('');
        Reg.CloseKey;

        if cbTCShellDirDefault.Checked then  //register as Directory's default action
        begin
          if Reg.OpenKey('Directory\shell', False) then
            Reg.WriteString('', 'wincmd')
          else
            Reg.WriteString('', 'none');
        end;
      end;

      if cbTCAnyFile.Checked and TCPathValid then
      begin
        if Reg.OpenKey(AnyFileTCShellFull,True) then Reg.WriteString('',opencmd);
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
        if Reg.OpenKey(DriveTCShellFull,True) then Reg.WriteString('',opencmd);
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
        tempstr := Application.ExeName + ' "%1"';
        if Reg.OpenKey(LinkTCShellFull,True) then Reg.WriteString('',tempstr);
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

    //now the archives
    if TCPathValid then
    for i := 0 to lvArchiveExts.Items.Count-1 do
      begin
        ext := lvArchiveExts.Items[i].Caption;
        if Reg.OpenKey('.'+ext, true) then
        begin
          typename := Reg.ReadString('');
          Reg.CloseKey;

          if ''=typename then
            typename:='.'+ext
          else
            if Reg.OpenKey(typename, true) then
            begin
              Reg.WriteString('', lvArchiveExts.Items[i].SubItems[COL_DESC]);
              Reg.CloseKey;
            end;

          if Reg.OpenKey(typename + '\shell\open\command', true) then
          begin
            if lvArchiveExts.Items[i].Checked then
            begin
              tempstr := Reg.ReadString('');
              Reg.WriteString('', opencmd);
              //if current command is not Total Commander
              if (''<>tempstr) and (Pos(UpperCase('totalcmd'), UpperCase(tempstr))=0) then
                Reg.WriteString('tcshell_backup', tempstr);
            end
            else
            begin //not checked
              if Reg.ValueExists('tcshell_backup') then
              begin
                tempstr := Reg.ReadString('tcshell_backup');
                //if old is not Total Commander
                if (''<>tempstr) and (Pos(UpperCase('totalcmd'), UpperCase(tempstr))=0) then
                begin
                  Reg.WriteString('', tempstr);
                  Reg.DeleteValue('tcshell_backup');
                end
              end
              else
              begin
                tempstr := Reg.ReadString('');
                //if associated with TotalCmd
                if Pos(tempstr, 'totalcmd.exe')>0 then
                begin
                  Reg.DeleteValue('');
                end;
              end;
            end;

            Reg.CloseKey;
          end;
        end;
      end;
    finally
      Reg.Free;
      CheckTCPackers;  //refresh
    end;
end;

procedure TfrmMain.btSelectTCPathClick(Sender: TObject);
begin
  OpenDlg.FileName := TCFullPath;
  OpenDlg.Filter := cDialogTCFileNameFilter;
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
//  rgTCInstance.Enabled := cbTCShellDir.Checked or cbTCShellDrive.Checked
//    or cbTCAnyFile.Checked or cbProcessShellLink.Checked or cbSentToLink.Checked;
//  rgTCPanel.Enabled := rgTCInstance.Enabled;
//  rgTCTab.Enabled := rgTCInstance.Enabled;
  cbOverrideMenuCap.Enabled := cbTCShellDir.Checked or cbTCShellDrive.Checked
    or cbTCAnyFile.Checked or cbProcessShellLink.Checked;
  edMenuCaption.Enabled := cbOverrideMenuCap.Checked and cbOverrideMenuCap.Enabled;
  cbProcessShellLink.Enabled := not cbTCAnyFile.Checked; // and cbTCShellDir.Checked;
  if cbTCAnyFile.Checked then
    cbProcessShellLink.Checked := False;
  
  cbTCShellDirDefault.Enabled := cbTCShellDir.Checked;

  edINI.Enabled := cbINI.Checked;
  btINI.Enabled := cbINI.Checked;
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
var
  cMail : string;
begin
  cMail := (Sender As TControl).Hint;
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

  GenerateCmdLineOptions;
  CheckTCShell;
  CheckTCPackers;
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

procedure TfrmMain.rgTCInstanceClick(Sender: TObject);
begin
  GenerateCmdLineOptions;
end;

procedure TfrmMain.btINIClick(Sender: TObject);
begin
  OpenDlg.InitialDir := ExtractFilePath(edINI.Text);
  OpenDlg.FileName := ExtractFileName(edINI.Text);
  OpenDlg.Filter := cDialogINIFileNameFilter;
  if OpenDlg.Execute then
  begin
    edINI.Text := OpenDlg.FileName;
    edINI.Hint := 'manually specified';
    GenerateCmdLineOptions;
  end;
end;

procedure TfrmMain.cbINIClick(Sender: TObject);
begin
  cbTCShellDirClick(cbTCShellDir);

  if (cbINI.Checked and (edINI.Text='')) or not cbINI.Checked then
  begin
    GuessDefaultIniPath;  
  end;

  GenerateCmdLineOptions;
  CheckTCPackers;
end;

procedure TfrmMain.edINIChange(Sender: TObject);
begin
  GenerateCmdLineOptions;
  CheckTCPackers;
end;

procedure TfrmMain.GuessDefaultIniPath;
var
  reg: TRegistry;
  inipath : string;
  hintstr : string;
begin
    //guess the current wincmd.ini
    inipath := GetEnvironmentVariable('COMMANDER_INI');
    hintstr := 'taken from %COMMANDER_INI%';
    if inipath='' then
    begin
      Reg := TRegistry.Create;
      try
        Reg.RootKey := HCU;
        if Reg.OpenKeyReadOnly('Software\Ghisler\Total Commander') then
        begin
          if Reg.ValueExists('IniFileName') then
          begin
            inipath := Reg.ReadString('IniFileName');
            hintstr := 'taken from registry HKCU\Software\Ghisler\Total Commander\IniFileName';
          end;
        end;
      finally
        Reg.Free;
      end;
    end;

    if (''=inipath) then  //this should not happen
    begin
        inipath := TCPath + 'wincmd.ini';
        hintstr := 'defaults to %COMMANDER_PATH%\wincmd.ini';
    end;

    edINI.Hint := hintstr;
    edINI.Text := inipath;
end;

procedure TfrmMain.lvArchiveExtsDblClick(Sender: TObject);
var
  item: TListItem;
  desc: string;
begin
  item := lvArchiveExts.Selected;
  if item<>nil then
  begin
    desc := item.SubItems[COL_DESC];
    desc := InputBox('TCShell2',
        'Set description for ' + item.Caption,
        desc);
    item.SubItems[COL_DESC] := desc;
  end;
end;

end.
