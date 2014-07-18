object Form1: TForm1
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'BSA extractor for TES 3,4,5 by Cynic v 1.1'
  ClientHeight = 377
  ClientWidth = 664
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object ListView1: TListView
    Left = 234
    Top = 8
    Width = 412
    Height = 330
    Columns = <
      item
        Caption = 'Filename'
        Width = 140
      end
      item
        Caption = 'FileSize'
        Width = 100
      end
      item
        Caption = 'Offset'
        Width = 110
      end>
    PopupMenu = ExtractMenu
    TabOrder = 0
    ViewStyle = vsReport
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 358
    Width = 664
    Height = 19
    Panels = <
      item
        Width = 120
      end
      item
        Width = 120
      end
      item
        Width = 160
      end
      item
        Width = 50
      end>
  end
  object TestTree: TTreeView
    Left = 8
    Top = 8
    Width = 220
    Height = 330
    Indent = 19
    PopupMenu = PopupExtract
    TabOrder = 2
    OnChange = TestTreeChange
    OnMouseDown = TestTreeMouseDown
  end
  object FileDlg: TOpenDialog
    Filter = 'Morrowind,Oblivion,Skyrim,Fallout 3|*.bsa'
    Left = 112
    Top = 88
  end
  object MainMenu1: TMainMenu
    Left = 40
    Top = 40
    object File1: TMenuItem
      Caption = 'File'
      object N2: TMenuItem
        Caption = 'Open'
        OnClick = N2Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = Exit1Click
      end
    end
  end
  object ExtractMenu: TPopupMenu
    Left = 184
    Top = 200
    object Extract1: TMenuItem
      Caption = 'Extract'
      OnClick = Extract1Click
    end
    object Extractall1: TMenuItem
      Caption = 'Extract all'
      Visible = False
      OnClick = Extractall1Click
    end
  end
  object PopupExtract: TPopupMenu
    Left = 232
    Top = 208
    object ExtractDirectory1: TMenuItem
      Caption = 'Extract Directory'
      OnClick = ExtractDirectory1Click
    end
    object ExctractAll1: TMenuItem
      Caption = 'Extract All'
      OnClick = Extractall1Click
    end
  end
end
