object ZUpdateSQLEditForm: TZUpdateSQLEditForm
  Left = 86
  Height = 258
  Top = 86
  Width = 401
  Caption = 'ZUpdateSQLEditForm'
  ClientHeight = 258
  ClientWidth = 401
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  Position = poScreenCenter
  LCLVersion = '7.4'
  object PageControl: TPageControl
    Left = 0
    Height = 221
    Top = 0
    Width = 401
    ActivePage = SQLPage
    Align = alClient
    TabIndex = 1
    TabOrder = 0
    OnChanging = PageControlChanging
    object FieldsPage: TTabSheet
      Caption = 'Options'
      ClientHeight = 195
      ClientWidth = 390
      object GroupBox1: TGroupBox
        Left = 0
        Height = 195
        Top = 0
        Width = 390
        Align = alClient
        Caption = ' SQL Generation '
        ClientHeight = 177
        ClientWidth = 386
        TabOrder = 0
        object Label1: TLabel
          Left = 16
          Height = 14
          Top = 8
          Width = 62
          Caption = 'Table &Name:'
          FocusControl = UpdateTableName
          ParentColor = False
        end
        object Label3: TLabel
          Left = 143
          Height = 14
          Top = 8
          Width = 52
          Anchors = [akTop, akRight]
          Caption = '&Key Fields:'
          FocusControl = KeyFieldList
          ParentColor = False
        end
        object Label4: TLabel
          Left = 265
          Height = 14
          Top = 8
          Width = 69
          Anchors = [akTop, akRight]
          Caption = 'Update &Fields:'
          FocusControl = UpdateFieldList
          ParentColor = False
        end
        object QuoteFields: TCheckBox
          Left = 16
          Height = 17
          Top = 152
          Width = 110
          Caption = '&Quote Field Names'
          OnClick = SettingsChanged
          TabOrder = 5
        end
        object UpdateTableName: TComboBox
          Left = 16
          Height = 21
          Top = 24
          Width = 113
          ItemHeight = 13
          OnChange = UpdateTableNameChange
          OnClick = UpdateTableNameClick
          TabOrder = 0
        end
        object GenerateButton: TButton
          Left = 16
          Height = 22
          Top = 120
          Width = 113
          Caption = '&Generate SQL'
          OnClick = GenerateButtonClick
          TabOrder = 4
        end
        object PrimaryKeyButton: TButton
          Left = 16
          Height = 22
          Top = 96
          Width = 113
          Caption = 'Select &Primary Keys'
          OnClick = PrimaryKeyButtonClick
          TabOrder = 3
        end
        object DefaultButton: TButton
          Left = 16
          Height = 21
          Top = 72
          Width = 113
          Caption = '&Dataset Defaults'
          Enabled = False
          OnClick = DefaultButtonClick
          TabOrder = 2
        end
        object GetTableFieldsButton: TButton
          Left = 16
          Height = 21
          Top = 48
          Width = 113
          Caption = 'Get &Table Fields'
          OnClick = GetTableFieldsButtonClick
          TabOrder = 1
        end
        object UpdateFieldList: TListBox
          Left = 264
          Height = 144
          Top = 24
          Width = 118
          Anchors = [akTop, akRight, akBottom]
          ItemHeight = 0
          MultiSelect = True
          OnClick = SettingsChanged
          PopupMenu = FieldListPopup
          TabOrder = 7
        end
        object KeyFieldList: TListBox
          Left = 136
          Height = 144
          Top = 24
          Width = 117
          Anchors = [akTop, akLeft, akRight, akBottom]
          ItemHeight = 0
          MultiSelect = True
          OnClick = SettingsChanged
          PopupMenu = FieldListPopup
          TabOrder = 6
        end
      end
    end
    object SQLPage: TTabSheet
      Caption = 'SQL'
      ClientHeight = 195
      ClientWidth = 393
      object Label2: TLabel
        Left = 8
        Height = 13
        Top = 40
        Width = 48
        Caption = 'S&QL Text:'
        FocusControl = SQLMemo
        ParentColor = False
      end
      object SQLMemo: TMemo
        Left = 8
        Height = 129
        Top = 56
        Width = 376
        Align = alCustom
        Anchors = [akTop, akLeft, akRight, akBottom]
        OnKeyPress = SQLMemoKeyPress
        ScrollBars = ssVertical
        TabOrder = 0
        WordWrap = False
      end
      object StatementType: TRadioGroup
        Left = 0
        Height = 35
        Top = 0
        Width = 393
        Align = alTop
        AutoFill = True
        Caption = 'Statement Type'
        ChildSizing.LeftRightSpacing = 6
        ChildSizing.TopBottomSpacing = 6
        ChildSizing.EnlargeHorizontal = crsHomogenousChildResize
        ChildSizing.EnlargeVertical = crsHomogenousChildResize
        ChildSizing.ShrinkHorizontal = crsScaleChilds
        ChildSizing.ShrinkVertical = crsScaleChilds
        ChildSizing.Layout = cclLeftToRightThenTopToBottom
        ChildSizing.ControlsPerLine = 3
        ClientHeight = 17
        ClientWidth = 389
        Columns = 3
        Items.Strings = (
          '&Modify'
          '&Insert'
          '&Delete'
        )
        OnClick = StatementTypeClick
        TabOrder = 1
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Height = 37
    Top = 221
    Width = 401
    Align = alBottom
    BevelOuter = bvNone
    ClientHeight = 37
    ClientWidth = 401
    TabOrder = 1
    object OkButton: TButton
      Left = 171
      Height = 22
      Top = 8
      Width = 65
      Anchors = [akRight, akBottom]
      Caption = '&OK'
      Default = True
      ModalResult = 1
      OnClick = OkButtonClick
      TabOrder = 0
    end
    object CancelButton: TButton
      Left = 248
      Height = 22
      Top = 7
      Width = 65
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object HelpButton: TButton
      Left = 324
      Height = 22
      Top = 7
      Width = 65
      Anchors = [akRight, akBottom]
      Caption = '&Help'
      OnClick = HelpButtonClick
      TabOrder = 2
    end
  end
  object FieldListPopup: TPopupMenu
    Left = 54
    Top = 270
    object miSelectAll: TMenuItem
      Caption = '&Select All'
      OnClick = SelectAllClick
    end
    object miClearAll: TMenuItem
      Caption = '&Clear All'
      OnClick = ClearAllClick
    end
  end
end
