unit ULinkProcess;

interface

procedure ProcessLinkFile(FileName: string);
function GetParamsFromLink(JustName: string): string;
function CreateOurLink(JustName, TCPath, Arguments: string): Boolean;
procedure DeleteOurLink(JustName: string);
function OurLinkExists(JustName: string): Boolean;
procedure TryToLocateFileInTC(fullpath: string);

implementation

uses Windows, SysUtils, ActiveX, ShlObj, ComObj, Registry, StrUtils, UMain,
     ShellApi, Messages, SndKey32;

procedure RunTC(WithPath: string);
var
  Reg: TRegistry;
  Str: string;
  APos: Integer;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HCR;
    if Reg.OpenKeyReadOnly(DirTCShellFull) then
    begin
      Str := Reg.ReadString('');
      if FileExists(ExtractFilePath(Str)+'totalcmd.exe') then
      begin
        APos := AnsiPos('%1',Str);
        if APos <> 0 then
        begin
          Str := AnsiReplaceText(Str,'%1',WithPath);
          WinExec(PChar(Str),SW_SHOW);
        end;
      end;
    end;
  finally
    Reg.Free;
  end;
end;

procedure ProcessLinkFile(FileName: string);
var
  SL: IShellLink;
  PF: IPersistFile;
  AStr: array[0..260] of Char;
  FindData: TWin32FindData;
  PathName: string;
begin
  if not FileExists(FileName) then Exit;
  if not Succeeded(CoInitialize(nil)) then Exit;
  try
    try
      OleCheck(CoCreateInstance(CLSID_ShellLink, nil, CLSCTX_INPROC_SERVER,IShellLink, SL));
      PF := SL as IPersistFile;
      OleCheck(PF.Load(PWideChar(WideString(FileName)), STGM_READ));
      OleCheck(SL.Resolve(0,SLR_ANY_MATCH or SLR_NO_UI));
      OleCheck(SL.GetPath(AStr, 260, FindData, SLGP_UNCPRIORITY{SLGP_SHORTPATH}));
      PathName := ExtractFilePath(AStr);
      RunTC(PathName);
    except
      //on E:Exception do MessageBox(0,PChar(E.Message),nil,MB_ICONINFORMATION);
    end;
  finally
    CoUninitialize;
  end;
end;

function GetParamsFromLink(JustName: string): string;
var
  SL: IShellLink;
  PF: IPersistFile;
  AStr: array[0..260] of Char;
  PathName: string;
begin
  Result := '';
  FillChar(AStr,SizeOf(AStr),0);
  if not SHGetSpecialFolderPath(frmMain.Handle,AStr,CSIDL_SENDTO,False) then Exit;
  PathName := IncludeTrailingPathDelimiter(AStr) + JustName + '.lnk';
  if not FileExists(PathName) then Exit;
  try
    try
      OleCheck(CoCreateInstance(CLSID_ShellLink, nil, CLSCTX_INPROC_SERVER,IShellLink, SL));
      PF := SL as IPersistFile;
      OleCheck(PF.Load(PWideChar(WideString(PathName)), STGM_READ));
      OleCheck(SL.Resolve(0,SLR_ANY_MATCH or SLR_NO_UI));
      FillChar(AStr,SizeOf(AStr),0);
      OleCheck(SL.GetArguments(AStr, 260));
      Result := AStr;
    except
      //on E:Exception do MessageBox(0,PChar(E.Message),nil,MB_ICONINFORMATION);
    end;
  finally
  end;
end;

function CreateOurLink(JustName, TCPath, Arguments: string): Boolean;
var
  SL: IShellLink;
  PF: IPersistFile;
  AStr: array[0..260] of Char;
  PathName: string;
begin
  Result := False;
  FillChar(AStr,SizeOf(AStr),0);
  if not SHGetSpecialFolderPath(frmMain.Handle,AStr,CSIDL_SENDTO,True) then Exit;
  PathName := IncludeTrailingPathDelimiter(AStr) + JustName + '.lnk';
  try
    try
      OleCheck(CoCreateInstance(CLSID_ShellLink, nil, CLSCTX_INPROC_SERVER,IShellLink, SL));
      PF := SL as IPersistFile;

      OleCheck(SL.SetDescription(PChar(JustName)));
      OleCheck(SL.SetPath(PChar(TCPath)));
      OleCheck(SL.SetArguments(PChar(Arguments)));
      OleCheck(SL.SetIconLocation(PChar(TCPath),0));

      OleCheck(PF.Save(PWideChar(WideString(PathName)),True));

      Result := True;
    except
      //on E:Exception do MessageBox(0,PChar(E.Message),nil,MB_ICONINFORMATION);
    end;
  finally
  end;
end;

procedure DeleteOurLink(JustName: string);
var
  AStr: array[0..260] of Char;
  PathName: string;
begin
  FillChar(AStr,SizeOf(AStr),0);
  if not SHGetSpecialFolderPath(frmMain.Handle,AStr,CSIDL_SENDTO,False) then Exit;
  PathName := IncludeTrailingPathDelimiter(AStr) + JustName + '.lnk';
  if FileExists(PathName) then DeleteFile(PathName);
end;

function OurLinkExists(JustName: string): Boolean;
var
  AStr: array[0..260] of Char;
  PathName: string;
begin
  Result := False;
  FillChar(AStr,SizeOf(AStr),0);
  if not SHGetSpecialFolderPath(frmMain.Handle,AStr,CSIDL_SENDTO,False) then Exit;
  PathName := IncludeTrailingPathDelimiter(AStr) + JustName + '.lnk';
  Result := FileExists(PathName);
end;

procedure TryToLocateFileInTC(fullpath: string);
var
  htcmd: HWND; //the HWND of Total Commander
  pbuf: PChar;
  
    function FindFileListBox(var idx: Integer): HWND;
    var
      title: array[0..1024] of char;
      hlist: HWND;
      count, row, len: integer;
      basename: string;
    begin
      Result := 0;
      idx := -1;

      basename := ExtractFileName(fullpath);

      hlist := FindWindowEx(htcmd, 0, 'TMyListBox', nil);
      while (hlist<>0) do
      begin
        GetWindowText(hlist, title, 1024);
        if StrLen(title)=0 then
        begin
          count := SendMessage(hlist, LB_GETCOUNT, 0, 0);
          for row := 0 to count-1 do
          begin
            len := SendMessage(hlist, LB_GETTEXTLEN, row, 0);
            GetMem(pbuf, len+1);
            try
                if LB_ERR<>SendMessage(hlist, LB_GETTEXT, row, Integer(pbuf)) then
                  if SameText(basename, Copy(StrPas(pbuf), 0, Length(basename))) then
                  begin
                     idx := row;
                     Result := hlist;
                     exit;
                  end;

            finally
              Dispose(pbuf);
            end;
          end;
        end;

        //try next listbox
        hlist := FindWindowEx(htcmd, hlist, 'TMyListBox', nil);
      end;

    end;
var
  idx, row: Integer;
  hlistbox: HWND;
  title: array[0..1024] of char;
begin
  //don't bother with directory
  if DirectoryExists(fullpath) then exit;

  htcmd := FindWindow('TTOTAL_CMD', nil);
  if (0=htcmd) then  exit;
  GetWindowText(htcmd, title, 1024);

  idx := -1;
  hlistbox := FindFileListBox(idx);
  if (hlistbox=0) or (idx<0) then exit;

  AppActivate(title);
  //FIXME: the key can only be sent to the source list, not the target list
  //    so it won't work, if '/S /R' used to invoke TC,
  SendKeys('{HOME}', true);
  for row := 0 to idx-1 do
      SendKeys('{DOWN}', true);
end;

end.
