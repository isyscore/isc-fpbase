object frmPropertyEditor: TfrmPropertyEditor
  Left = 86
  Height = 561
  Top = 86
  Width = 637
  Caption = 'Edit Properties ...'
  ClientHeight = 561
  ClientWidth = 637
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  Position = poScreenCenter
  LCLVersion = '7.3'
  object pcEditValues: TPageControl
    Left = 0
    Height = 487
    Top = 0
    Width = 637
    ActivePage = tcDiffList
    Align = alTop
    Anchors = [akTop, akLeft, akRight, akBottom]
    TabIndex = 0
    TabOrder = 0
    OnChanging = pcEditValuesChanging
    object tcDiffList: TTabSheet
      Caption = 'Diff-List'
      ClientHeight = 461
      ClientWidth = 629
      object Splitter1: TSplitter
        Cursor = crVSplit
        Left = 0
        Height = 5
        Top = 280
        Width = 629
        Align = alBottom
        ResizeAnchor = akBottom
      end
      object pnlProps: TPanel
        Left = 0
        Height = 280
        Top = 0
        Width = 629
        Align = alClient
        BevelOuter = bvNone
        BorderWidth = 8
        ClientHeight = 280
        ClientWidth = 629
        TabOrder = 0
        object spltProps: TSplitter
          Left = 305
          Height = 264
          Top = 8
          Width = 4
        end
        object bgPropsUsed: TGroupBox
          Left = 8
          Height = 264
          Top = 8
          Width = 297
          Align = alLeft
          Caption = 'Used properties'
          ClientHeight = 246
          ClientWidth = 293
          TabOrder = 0
          object lbUsed: TListBox
            Left = 8
            Height = 222
            Top = 16
            Width = 245
            Anchors = [akTop, akLeft, akRight, akBottom]
            ItemHeight = 0
            OnClick = lbUsedClick
            OnMouseMove = lbUsedMouseMove
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
          end
          object btnAdd: TButton
            Left = 260
            Height = 25
            Top = 16
            Width = 25
            Anchors = [akTop, akRight]
            Caption = '<'
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            OnClick = btnAddClick
            ParentFont = False
            TabOrder = 1
          end
          object btnRemove: TButton
            Left = 260
            Height = 25
            Top = 48
            Width = 25
            Anchors = [akTop, akRight]
            Caption = '>'
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            OnClick = btnRemoveClick
            ParentFont = False
            TabOrder = 2
          end
        end
        object gbAvailable: TGroupBox
          Left = 309
          Height = 264
          Top = 8
          Width = 312
          Align = alClient
          Caption = 'Available properties'
          ClientHeight = 246
          ClientWidth = 308
          TabOrder = 1
          object lbAvailable: TListBox
            Left = 8
            Height = 222
            Top = 15
            Width = 293
            Anchors = [akTop, akLeft, akRight, akBottom]
            ItemHeight = 0
            OnClick = lbAvailableClick
            OnMouseMove = lbAvailableMouseMove
            TabOrder = 0
          end
        end
      end
      object pnlBottom: TPanel
        Left = 0
        Height = 176
        Top = 285
        Width = 629
        Align = alBottom
        BevelOuter = bvNone
        ClientHeight = 176
        ClientWidth = 629
        TabOrder = 1
        object lblProtocol: TLabel
          Left = 8
          Height = 13
          Top = 144
          Width = 42
          Anchors = [akLeft, akBottom]
          Caption = 'Protocol:'
          ParentColor = False
        end
        object lblServerProvider: TLabel
          Left = 168
          Height = 13
          Top = 144
          Width = 73
          Anchors = [akLeft, akBottom]
          Caption = 'ServerProvider:'
          ParentColor = False
          Visible = False
        end
        object lblHostversion: TLabel
          Left = 424
          Height = 13
          Top = 144
          Width = 68
          Anchors = [akLeft, akBottom]
          Caption = 'Hostversion: 0'
          ParentColor = False
          Visible = False
        end
        object lblClientVersion: TLabel
          Left = 512
          Height = 13
          Top = 144
          Width = 73
          Anchors = [akLeft, akBottom]
          Caption = 'ClientVersion: 0'
          ParentColor = False
          Visible = False
        end
        object cbProtocol: TComboBox
          Left = 56
          Height = 21
          Top = 144
          Width = 105
          ItemHeight = 13
          OnChange = cbProtocolChange
          Style = csDropDownList
          TabOrder = 0
        end
        object pnlValDesc: TPanel
          Left = 0
          Height = 137
          Top = 0
          Width = 629
          Align = alTop
          BevelOuter = bvNone
          BorderWidth = 8
          ClientHeight = 137
          ClientWidth = 629
          TabOrder = 1
          object Splitter2: TSplitter
            Left = 305
            Height = 121
            Top = 8
            Width = 5
          end
          object gbVal: TGroupBox
            Left = 8
            Height = 121
            Top = 8
            Width = 297
            Align = alLeft
            Caption = 'Value'
            ClientHeight = 103
            ClientWidth = 293
            TabOrder = 0
            object lblProtocols: TLabel
              Left = 8
              Height = 13
              Top = 40
              Width = 44
              Caption = 'Protocols'
              ParentColor = False
              Visible = False
              WordWrap = True
            end
            object lblProviders: TLabel
              Left = 140
              Height = 13
              Top = 40
              Width = 44
              Anchors = [akTop, akRight]
              Caption = 'Providers'
              ParentColor = False
              Visible = False
              WordWrap = True
            end
            object cbEnum: TComboBox
              Left = 8
              Height = 21
              Top = 88
              Width = 277
              Anchors = [akTop, akLeft, akRight]
              ItemHeight = 13
              Style = csDropDownList
              TabOrder = 0
            end
            object edString: TEdit
              Left = 8
              Height = 21
              Top = 16
              Width = 277
              Anchors = [akTop, akLeft, akRight]
              TabOrder = 1
            end
          end
          object gbDescription: TGroupBox
            Left = 310
            Height = 121
            Top = 8
            Width = 311
            Align = alClient
            Caption = 'Purpose/Description'
            ClientHeight = 103
            ClientWidth = 307
            TabOrder = 1
            object mmDescrption: TMemo
              Left = 8
              Height = 79
              Top = 16
              Width = 291
              Anchors = [akTop, akLeft, akRight, akBottom]
              ReadOnly = True
              ScrollBars = ssVertical
              TabOrder = 0
            end
          end
        end
        object cbProvider: TComboBox
          Left = 248
          Height = 21
          Top = 144
          Width = 113
          ItemHeight = 13
          Style = csDropDownList
          TabOrder = 2
        end
      end
    end
    object tsStringList: TTabSheet
      Caption = 'String-List'
      ClientHeight = 0
      ClientWidth = 0
      ImageIndex = 1
      object mmStringList: TMemo
        Left = 8
        Height = 443
        Top = 8
        Width = 581
        Anchors = [akTop, akLeft, akRight, akBottom]
        TabOrder = 0
      end
    end
  end
  object btnOK: TButton
    Left = 457
    Height = 25
    Top = 491
    Width = 75
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 537
    Height = 25
    Top = 491
    Width = 75
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object cbHideAlias: TCheckBox
    Left = 16
    Height = 19
    Top = 496
    Width = 183
    Alignment = taLeftJustify
    Caption = 'Hide properties with equal purpose'
    OnClick = cbHideAliasClick
    TabOrder = 3
  end
end
