object frmMain: TfrmMain
  Left = 322
  Top = 126
  Width = 531
  Height = 529
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'frmMain'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  OnCreate = FormCreate
  OnPaint = FormPaint
  DesignSize = (
    523
    495)
  PixelsPerInch = 96
  TextHeight = 13
  object lblTCPath: TLabel
    Left = 16
    Top = 13
    Width = 113
    Height = 13
    Caption = 'Total Commander Path:'
    FocusControl = edTCPath
  end
  object Panel2: TPanel
    Left = 0
    Top = 455
    Width = 523
    Height = 40
    Align = alBottom
    BevelOuter = bvNone
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 0
    DesignSize = (
      523
      40)
    object btOK: TButton
      Left = 261
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = btOKClick
    end
    object btCancel: TButton
      Left = 342
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
      OnClick = btCancelClick
    end
    object btnApply: TButton
      Left = 423
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Apply'
      ModalResult = 2
      TabOrder = 2
      OnClick = btnApplyClick
    end
  end
  object pgc1: TPageControl
    Left = 8
    Top = 88
    Width = 504
    Height = 365
    ActivePage = ts3
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
    object ts1: TTabSheet
      Caption = 'Context Menu'
      DesignSize = (
        496
        337)
      object lblMail: TLabel
        Left = 326
        Top = 284
        Width = 96
        Height = 13
        Cursor = crHandPoint
        Hint = 
          'mailto:ProgMan13@mail.ru?subject="TC in Shell menu 1.3.5 Modifie' +
          'd"'
        Anchors = [akRight, akBottom]
        Caption = 'ProgMan13@mail.ru'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsUnderline]
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        OnClick = lblMailClick
        OnMouseDown = lblMailMouseDown
        OnMouseUp = lblMailMouseUp
        OnMouseEnter = lblMailMouseEnter
        OnMouseLeave = lblMailMouseLeave
      end
      object Label1: TLabel
        Left = 238
        Top = 284
        Width = 78
        Height = 13
        Alignment = taRightJustify
        Anchors = [akRight, akBottom]
        Caption = 'Original author: '
      end
      object lblAuthor2: TLabel
        Left = 254
        Top = 308
        Width = 62
        Height = 13
        Alignment = taRightJustify
        Anchors = [akRight, akBottom]
        Caption = 'Modified by: '
      end
      object lblMain2: TLabel
        Left = 326
        Top = 308
        Width = 97
        Height = 13
        Cursor = crHandPoint
        Hint = 'mailto:bamanzi@gmail.com?subject="As per TCShell2"'
        Anchors = [akRight, akBottom]
        Caption = 'bamanzi@gmail.com'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsUnderline]
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        OnClick = lblMailClick
        OnMouseDown = lblMailMouseDown
        OnMouseUp = lblMailMouseUp
        OnMouseEnter = lblMailMouseEnter
        OnMouseLeave = lblMailMouseLeave
      end
      object lblShellLinkHint: TLabel
        Left = 48
        Top = 107
        Width = 398
        Height = 13
        Caption = 
          'Note: check this option would require that you keep TCShell2 whe' +
          're it running from'
      end
      object cbTCShellDir: TCheckBox
        Left = 8
        Top = 18
        Width = 377
        Height = 17
        Caption = 'Add "Open in Total Commander" for Directory context menu'
        State = cbGrayed
        TabOrder = 0
        OnClick = cbTCShellDirClick
      end
      object cbTCShellDrive: TCheckBox
        Left = 8
        Top = 64
        Width = 377
        Height = 17
        Caption = 'Add "Open in Total Commander" for Drive context menu'#39
        State = cbGrayed
        TabOrder = 1
        OnClick = cbTCShellDirClick
      end
      object cbProcessShellLink: TCheckBox
        Left = 8
        Top = 86
        Width = 377
        Height = 17
        Hint = 'Add "Open in Total Commander" for Shell Link context menu'
        Caption = 'Add "Open in Total Commander" for Shell Link context menu'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        OnClick = cbTCShellDirClick
      end
      object cbTCAnyFile: TCheckBox
        Left = 8
        Top = 132
        Width = 385
        Height = 17
        Caption = 'Add "Open in Total Commander" for ANY FILE context menu'
        TabOrder = 3
        OnClick = cbTCShellDirClick
      end
      object cbOverrideMenuCap: TCheckBox
        Left = 8
        Top = 156
        Width = 129
        Height = 16
        Caption = 'Override menu caption'
        TabOrder = 4
        OnClick = cbOverrideMenuCapClick
      end
      object edMenuCaption: TEdit
        Left = 160
        Top = 153
        Width = 307
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        Enabled = False
        TabOrder = 5
      end
      object cbSentToLink: TCheckBox
        Left = 8
        Top = 179
        Width = 377
        Height = 17
        Caption = 'Link "Open in Total Commander" in "Send To" menu'
        TabOrder = 6
      end
      object cbTCShellDirDefault: TCheckBox
        Left = 40
        Top = 40
        Width = 297
        Height = 17
        Hint = 
          'When checked, if you launch a directory in Windows Run dialog, o' +
          'r call ShellExecute on a directory, '#13#10'it would be opened in Tota' +
          'l Command'
        Caption = 'Register as default action  for Directory (experimental!!)'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 7
        OnClick = cbTCShellDirClick
      end
    end
    object ts2: TTabSheet
      Caption = 'Archives'
      ImageIndex = 1
      DesignSize = (
        496
        337)
      object Label8: TLabel
        Left = 8
        Top = 8
        Width = 256
        Height = 13
        Caption = 'Use Total Commander to open the following archives:'
      end
      object lvArchiveExts: TListView
        Left = 8
        Top = 32
        Width = 473
        Height = 293
        Anchors = [akLeft, akTop, akRight, akBottom]
        Checkboxes = True
        Columns = <
          item
            Caption = 'ext'
            Width = 80
          end
          item
            Caption = 'unpacker'
            Width = 80
          end
          item
            Caption = 'desc'
            Width = 100
          end
          item
            Caption = 'association'
            Width = 200
          end>
        Enabled = False
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnDblClick = lvArchiveExtsDblClick
      end
    end
    object ts3: TTabSheet
      Caption = 'Options'
      ImageIndex = 2
      DesignSize = (
        496
        337)
      object rgTCInstance: TRadioGroup
        Left = 16
        Top = 8
        Width = 400
        Height = 57
        Anchors = [akLeft, akTop, akRight]
        Caption = 'How TC can open new dir\drive'
        ItemIndex = 1
        Items.Strings = (
          'Always in new TC instanse'
          'Always in current TC instance')
        TabOrder = 0
        OnClick = rgTCInstanceClick
      end
      object rgTCPanel: TRadioGroup
        Left = 16
        Top = 72
        Width = 400
        Height = 73
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Select TC panel where dir\drive will be opened '
        Columns = 2
        ItemIndex = 2
        Items.Strings = (
          'Open in Left panel'
          'Open in Right panel'
          'Open in Default panel'
          'Open in Source panel'
          'Open in Target Panel')
        TabOrder = 1
        OnClick = rgTCInstanceClick
      end
      object rgTCTab: TRadioGroup
        Left = 16
        Top = 152
        Width = 400
        Height = 57
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Select TC tab where dir\drive will be opened (TC 6.50 or later)'
        ItemIndex = 1
        Items.Strings = (
          'Open in New tab'
          'Open in Default tab')
        TabOrder = 2
        OnClick = rgTCInstanceClick
      end
      object edtCmdLine: TEdit
        Left = 16
        Top = 232
        Width = 401
        Height = 21
        Color = clInactiveCaptionText
        ReadOnly = True
        TabOrder = 3
      end
    end
  end
  object edTCPath: TEdit
    Left = 16
    Top = 32
    Width = 478
    Height = 21
    TabStop = False
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 2
  end
  object btSelectTCPath: TButton
    Left = 421
    Top = 9
    Width = 75
    Height = 21
    Anchors = [akTop, akRight]
    Caption = 'Select...'
    TabOrder = 3
    OnClick = btSelectTCPathClick
  end
  object cbINI: TCheckBox
    Left = 16
    Top = 58
    Width = 41
    Height = 17
    Caption = 'INI'
    TabOrder = 4
    OnClick = cbINIClick
  end
  object edINI: TEdit
    Left = 64
    Top = 56
    Width = 390
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 5
    OnChange = edINIChange
  end
  object btINI: TButton
    Left = 466
    Top = 56
    Width = 27
    Height = 21
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 6
    OnClick = btINIClick
  end
  object OpenDlg: TOpenDialog
    Filter = 'totalcmd.exe|totalcmd.exe'
    Options = [ofReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing, ofDontAddToRecent, ofForceShowHidden]
    Left = 240
    Top = 8
  end
  object XPManifest1: TXPManifest
    Left = 208
    Top = 8
  end
end
