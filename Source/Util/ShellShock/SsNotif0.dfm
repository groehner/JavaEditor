object ShellNotifyEditorForm: TShellNotifyEditorForm
  Left = 498
  Top = 146
  BorderStyle = bsDialog
  Caption = 'TStShellNotification Component Editor'
  ClientHeight = 383
  ClientWidth = 436
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Default'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 1
    Top = 1
    Width = 434
    Height = 343
    Style = bsRaised
  end
  object Label1: TLabel
    Left = 7
    Top = 9
    Width = 64
    Height = 13
    Caption = '&Watch folder:'
  end
  object EventsGb: TGroupBox
    Left = 5
    Top = 152
    Width = 427
    Height = 185
    Caption = ' &Events '
    TabOrder = 1
    object AssociationCb: TCheckBox
      Left = 11
      Top = 16
      Width = 121
      Height = 17
      Caption = 'Association change'
      TabOrder = 0
    end
    object AttributeCb: TCheckBox
      Left = 11
      Top = 32
      Width = 121
      Height = 17
      Caption = 'Attribute change'
      TabOrder = 1
    end
    object FileCreateCb: TCheckBox
      Left = 11
      Top = 48
      Width = 121
      Height = 17
      Caption = 'File create'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
    object FileDeleteCb: TCheckBox
      Left = 11
      Top = 64
      Width = 121
      Height = 17
      Caption = 'File delete'
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
    object FileRenameCb: TCheckBox
      Left = 11
      Top = 80
      Width = 121
      Height = 17
      Caption = 'File rename'
      Checked = True
      State = cbChecked
      TabOrder = 4
    end
    object FolderRenameCb: TCheckBox
      Left = 11
      Top = 145
      Width = 121
      Height = 17
      Caption = 'Folder rename'
      Checked = True
      State = cbChecked
      TabOrder = 8
    end
    object FolderChangeCb: TCheckBox
      Left = 11
      Top = 161
      Width = 121
      Height = 17
      Caption = 'Folder change'
      TabOrder = 9
    end
    object FileChangeCb: TCheckBox
      Left = 11
      Top = 96
      Width = 121
      Height = 17
      Caption = 'File change'
      Checked = True
      State = cbChecked
      TabOrder = 5
    end
    object FolderCreateCb: TCheckBox
      Left = 11
      Top = 112
      Width = 121
      Height = 17
      Caption = 'Folder create'
      Checked = True
      State = cbChecked
      TabOrder = 6
    end
    object FolderDeleteCb: TCheckBox
      Left = 11
      Top = 128
      Width = 121
      Height = 17
      Caption = 'Folder delete'
      Checked = True
      State = cbChecked
      TabOrder = 7
    end
    object DriveAddCb: TCheckBox
      Left = 163
      Top = 16
      Width = 121
      Height = 17
      Caption = 'Drive add'
      TabOrder = 10
    end
    object DriveRemoveCb: TCheckBox
      Left = 163
      Top = 32
      Width = 121
      Height = 17
      Caption = 'Drive remove'
      TabOrder = 11
    end
    object ShellDriveAddCb: TCheckBox
      Left = 163
      Top = 48
      Width = 121
      Height = 17
      Caption = 'Drive add (shell)'
      TabOrder = 12
    end
    object NetShareCb: TCheckBox
      Left = 163
      Top = 64
      Width = 121
      Height = 17
      Caption = 'Net share'
      TabOrder = 13
    end
    object NetUnShareCb: TCheckBox
      Left = 163
      Top = 80
      Width = 121
      Height = 17
      Caption = 'Net unshare'
      TabOrder = 14
    end
    object FreeSpaceCb: TCheckBox
      Left = 163
      Top = 145
      Width = 145
      Height = 17
      Caption = 'Folder free space change'
      TabOrder = 18
    end
    object ImageListChangeCb: TCheckBox
      Left = 163
      Top = 161
      Width = 121
      Height = 17
      Caption = 'Image list change'
      TabOrder = 19
    end
    object ServerDisconnectCb: TCheckBox
      Left = 163
      Top = 96
      Width = 121
      Height = 17
      Caption = 'Server disconnect'
      TabOrder = 15
    end
    object MediaInsertCb: TCheckBox
      Left = 163
      Top = 112
      Width = 121
      Height = 17
      Caption = 'Media insert'
      TabOrder = 16
    end
    object MediaRemoveCb: TCheckBox
      Left = 163
      Top = 128
      Width = 121
      Height = 17
      Caption = 'Media remove'
      TabOrder = 17
    end
    object SelectAllBtn: TButton
      Left = 344
      Top = 14
      Width = 75
      Height = 25
      Caption = 'Select &All'
      TabOrder = 20
      OnClick = SelectAllBtnClick
    end
    object ClearAllBtn: TButton
      Left = 344
      Top = 41
      Width = 75
      Height = 25
      Caption = 'Clea&r All'
      TabOrder = 21
      OnClick = ClearAllBtnClick
    end
  end
  object OptionsGb: TGroupBox
    Left = 305
    Top = 20
    Width = 122
    Height = 61
    Caption = ' &Options '
    TabOrder = 0
    object WatchSubFoldersCb: TCheckBox
      Left = 10
      Top = 37
      Width = 105
      Height = 17
      Caption = 'Watch subfolders'
      TabOrder = 1
    end
    object ActiveCb: TCheckBox
      Left = 10
      Top = 18
      Width = 97
      Height = 17
      Caption = 'Active'
      TabOrder = 0
    end
  end
  object OKBtn: TButton
    Left = 281
    Top = 352
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 3
    OnClick = OKBtnClick
  end
  object CancelBtn: TButton
    Left = 359
    Top = 352
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = CancelBtnClick
  end
end
