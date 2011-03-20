program TCShell;

uses
  Forms,
  SysUtils,
  Windows,
  UMain in 'UMain.pas' {frmMain},
  UEngStrings in 'UEngStrings.pas',
  ULinkProcess in 'ULinkProcess.pas';

{$R *.res}

var
  FileName: string;

begin
  if ParamCount > 0 then
  begin
    FileName := AnsiDequotedStr(ParamStr(1),'"');
    if AnsiSameText(ExtractFileExt(FileName),'.lnk') then
    begin
      ProcessLinkFile(FileName);
      Exit;
    end;
  end;

  Application.Initialize;
  Application.Title := 'TC in Shell menu';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
