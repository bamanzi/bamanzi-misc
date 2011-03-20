object frmMain: TfrmMain
  Left = 240
  Top = 88
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'frmMain'
  ClientHeight = 426
  ClientWidth = 392
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnPaint = FormPaint
  PixelsPerInch = 96
  TextHeight = 13
  object lblTCPath: TLabel
    Left = 8
    Top = 5
    Width = 122
    Height = 13
    Caption = #1055#1091#1090#1100' '#1082' Total Commander:'
    FocusControl = edTCPath
  end
  object Panel2: TPanel
    Left = 0
    Top = 392
    Width = 392
    Height = 34
    Align = alBottom
    BevelOuter = bvNone
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 0
    object lblMail: TLabel
      Left = 32
      Top = 11
      Width = 96
      Height = 13
      Cursor = crHandPoint
      Hint = #1053#1072#1087#1080#1089#1072#1090#1100' '#1087#1080#1089#1100#1084#1086
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
    object btOK: TButton
      Left = 215
      Top = 5
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = btOKClick
    end
    object btCancel: TButton
      Left = 303
      Top = 5
      Width = 75
      Height = 25
      Cancel = True
      Caption = #1054#1090#1084#1077#1085#1072
      ModalResult = 2
      TabOrder = 1
      OnClick = btCancelClick
    end
  end
  object cbOverrideMenuCap: TCheckBox
    Left = 8
    Top = 140
    Width = 129
    Height = 16
    Caption = #1053#1072#1079#1074#1072#1085#1080#1077' '#1084#1077#1085#1102
    TabOrder = 5
    OnClick = cbOverrideMenuCapClick
  end
  object edMenuCaption: TEdit
    Left = 160
    Top = 137
    Width = 225
    Height = 21
    Enabled = False
    TabOrder = 6
  end
  object cbTCShellDrive: TCheckBox
    Left = 8
    Top = 72
    Width = 377
    Height = 17
    Caption = #1044#1086#1073#1072#1074#1080#1090#1100' "Open in Total Commander" '#1074' '#1082#1086#1085#1090#1077#1082#1089#1090#1085#1086#1077' '#1084#1077#1085#1102' '#1044#1080#1089#1082#1086#1074
    State = cbGrayed
    TabOrder = 3
    OnClick = cbTCShellDirClick
  end
  object cbTCShellDir: TCheckBox
    Left = 8
    Top = 50
    Width = 377
    Height = 17
    Caption = #1044#1086#1073#1072#1074#1080#1090#1100' "Open in Total Commander" '#1074' '#1082#1086#1085#1090#1077#1082#1089#1090#1085#1086#1077' '#1084#1077#1085#1102' '#1050#1072#1090#1072#1083#1086#1075#1086#1074
    State = cbGrayed
    TabOrder = 2
    OnClick = cbTCShellDirClick
  end
  object edTCPath: TEdit
    Left = 8
    Top = 24
    Width = 377
    Height = 21
    TabStop = False
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 8
  end
  object btSelectTCPath: TButton
    Left = 310
    Top = 1
    Width = 75
    Height = 21
    Caption = #1054#1073#1079#1086#1088'...'
    TabOrder = 1
    OnClick = btSelectTCPathClick
  end
  object cbProcessShellLink: TCheckBox
    Left = 8
    Top = 94
    Width = 377
    Height = 17
    Hint = 
      #1054#1073#1088#1072#1073#1086#1090#1082#1072' '#1087#1091#1090#1077#1081' '#1103#1088#1083#1099#1082#1072' '#1086#1089#1091#1097#1077#1089#1090#1074#1083#1103#1077#1090#1089#1103' '#1090#1086#1083#1100#1082#1086' '#1087#1088#1080' '#1085#1072#1083#1080#1095#1080#1080' '#1101#1090#1086#1081' '#1087#1088 +
      #1086#1075#1088#1072#1084#1084#1099'!'
    Caption = #1044#1086#1073#1072#1074#1080#1090#1100' "Open in Total Commander" '#1074' '#1082#1086#1085#1090#1077#1082#1089#1090#1085#1086#1077' '#1084#1077#1085#1102' '#1071#1088#1083#1099#1082#1086#1074
    ParentShowHint = False
    ShowHint = True
    TabOrder = 4
    OnClick = cbTCShellDirClick
  end
  object cbTCAnyFile: TCheckBox
    Left = 8
    Top = 116
    Width = 385
    Height = 17
    Caption = 
      #1044#1086#1073#1072#1074#1080#1090#1100' "Open in Total Commander" '#1074' '#1082#1086#1085#1090#1077#1082#1089#1090#1085#1086#1077' '#1084#1077#1085#1102' '#1074#1089#1077#1093' '#1092#1072#1081#1083#1086 +
      #1074'!'
    TabOrder = 9
    OnClick = cbTCShellDirClick
  end
  object Panel3: TGroupBox
    Left = 8
    Top = 184
    Width = 377
    Height = 209
    TabOrder = 7
    object rgTCInstance: TRadioGroup
      Left = 8
      Top = 8
      Width = 361
      Height = 57
      Caption = ' '#1050#1072#1082' TC '#1076#1086#1083#1078#1077#1085' '#1086#1090#1082#1088#1099#1074#1072#1090#1100' '#1085#1086#1074#1099#1077' '#1050#1072#1090#1072#1083#1086#1075#1080'\'#1044#1080#1089#1082#1080' '#1080#1079' '#1084#1077#1085#1102' '
      ItemIndex = 1
      Items.Strings = (
        #1042#1089#1077#1075#1076#1072' '#1086#1090#1082#1088#1099#1074#1072#1090#1100' '#1085#1086#1074#1086#1077' '#1086#1082#1085#1086' TC'
        #1042#1089#1077#1075#1076#1072' '#1079#1072#1075#1088#1091#1078#1072#1090#1100' '#1074' '#1089#1091#1097#1077#1089#1090#1074#1091#1102#1097#1077#1077' '#1086#1082#1085#1086' TC')
      TabOrder = 0
    end
    object rgTCPanel: TRadioGroup
      Left = 8
      Top = 72
      Width = 361
      Height = 65
      Caption = ' '#1042' '#1082#1072#1082#1086#1081' '#1092#1072#1081#1083#1086#1074#1086#1081' '#1087#1072#1085#1077#1083#1080' TC '#1086#1090#1082#1088#1099#1074#1072#1090#1100' '#1050#1072#1090#1072#1083#1086#1075#1080'\'#1044#1080#1089#1082#1080' '#1080#1079' '#1084#1077#1085#1102' '
      ItemIndex = 2
      Items.Strings = (
        #1054#1090#1082#1088#1099#1074#1072#1090#1100' '#1074' '#1051#1077#1074#1086#1081' '#1087#1072#1085#1077#1083#1080
        #1054#1090#1082#1088#1099#1074#1072#1090#1100' '#1074' '#1055#1088#1072#1074#1086#1081' '#1087#1072#1085#1077#1083#1080
        #1054#1090#1082#1088#1099#1074#1072#1090#1100' '#1074' '#1087#1072#1085#1077#1083#1080' '#1087#1086' '#1091#1084#1086#1083#1095#1072#1085#1080#1102)
      TabOrder = 1
    end
    object rgTCTab: TRadioGroup
      Left = 8
      Top = 144
      Width = 361
      Height = 57
      Caption = ' '#1053#1072' '#1082#1072#1082#1086#1081' '#1074#1082#1083#1072#1076#1082#1077' '#1086#1090#1082#1088#1099#1074#1072#1090#1100' (TC 6.50 '#1080#1083#1080' '#1085#1086#1074#1077#1077') '
      ItemIndex = 1
      Items.Strings = (
        #1042#1089#1077#1075#1076#1072' '#1086#1090#1082#1088#1099#1074#1072#1090#1100' '#1085#1072' '#1085#1086#1074#1086#1081' '#1074#1082#1083#1072#1076#1082#1077
        #1054#1090#1082#1088#1099#1074#1072#1090#1100' '#1085#1072' '#1074#1082#1083#1072#1076#1082#1077' '#1087#1086' '#1091#1084#1086#1083#1095#1072#1085#1080#1102)
      TabOrder = 2
    end
  end
  object cbSentToLink: TCheckBox
    Left = 8
    Top = 163
    Width = 377
    Height = 17
    Caption = #1071#1088#1083#1099#1082' "Open in Total Commander" '#1074' '#1084#1077#1085#1102' "'#1054#1090#1087#1088#1072#1074#1080#1090#1100'"'
    TabOrder = 10
    OnClick = cbTCShellDirClick
  end
  object OpenDlg: TOpenDialog
    Filter = 'totalcmd.exe|totalcmd.exe'
    Options = [ofReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing, ofDontAddToRecent, ofForceShowHidden]
    Left = 232
    Top = 8
  end
  object XPManifest1: TXPManifest
    Left = 200
    Top = 8
  end
end
