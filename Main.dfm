object FMain: TFMain
  Left = 1217
  Top = 124
  Width = 362
  Height = 792
  Caption = #1055#1086#1080#1089#1082' '#1082#1088#1072#1090#1095#1072#1081#1096#1077#1075#1086' '#1087#1091#1090#1080' '#1087#1086' '#1075#1088#1072#1092#1091
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 14
  object status: TuniStatusBar
    Left = 0
    Top = 734
    Width = 346
    Height = 19
    Panels = <
      item
        Text = #1042#1088#1077#1084#1103' '#1087#1086#1080#1089#1082#1072': '
        Enabled = True
        Width = 140
        AutoFit = False
        Hint = '...'
      end
      item
        Text = #1044#1083#1080#1085#1072' '#1087#1091#1090#1080':'
        Enabled = True
        Width = 50
        AutoFit = False
      end>
    SizeGrip = False
  end
  object GBStart: TGroupBox
    Left = 0
    Top = 0
    Width = 346
    Height = 145
    Align = alTop
    Caption = ' '#1053#1072#1095#1072#1083#1086' '#1087#1091#1090#1080': '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    DesignSize = (
      346
      145)
    object LStartStreet: TLabel
      Left = 14
      Top = 29
      Width = 36
      Height = 14
      Caption = #1059#1083#1080#1094#1072': '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object LStartHouse: TLabel
      Left = 14
      Top = 62
      Width = 28
      Height = 14
      Caption = #1044#1086#1084': '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object BSetStartAddress: TSpeedButton
      Left = 16
      Top = 96
      Width = 150
      Height = 25
      Caption = #1042#1099#1073#1088#1072#1090#1100' '#1072#1076#1088#1077#1089' '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      Glyph.Data = {
        36060000424D3606000000000000360000002800000020000000100000000100
        18000000000000060000C40E0000C40E00000000000000000000FF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF3A81AC3980AB
        3A81ACFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FF727272717171727272FF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF387FAB6BB2D4
        397FABFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FF7070709F9F9F717171FF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF387FAA69B0D3
        387FABFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FF7070709D9D9D707070FF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF387FAA69B0D3
        3A7EAFFF00FFFF00FFFF00FFFF00FFFF00FFF0F0F0CDCDCDCCCCCCCCCCCCCCCC
        CCDEDEDEF5F5F5FF00FF7070709D9D9D737373FF00FFFF00FFFF00FFFF00FFFF
        00FFF0F0F0CDCDCDCCCCCCCCCCCCCCCCCCDEDEDEF5F5F5FF00FF387FAB6BB0D6
        3F7DB6DFDFDFF2F2F2FF00FFFF00FFFF00FF92BBA9049051008D4C0089480088
        4746A076A2BFB2E0E0E0707070A0A0A0797979DFDFDFF2F2F2FF00FFFF00FFFF
        00FFA6A6A6494949464646444444434343727272B0B0B0E000E0397FAB66ACCF
        008C454EA57A96BBAACCCCCCCCCCCCCCCCCC008E4D008D4C008B4A00EFBF00E3
        AF00C48A00A36352A67F7171719A9A9A454545787878A8A8A8CCCCCCCCCCCCCC
        CCCC4646464646464545457777777171716262625151517B297B397FAC6FAEDC
        008A3E00A86500995A00884700884700884700874600985800844200EAB900DC
        A800CC9256D8AE008948717171A5A5A54444445353534C4C4C43434343434343
        43434343434B4B4B4141417575756E6E6E666666969696444444397FAC6EAEDC
        00883C54D4A800C58900D29A00D19B00CF9A00E1AD56DEB600813E00E9B700DB
        A600CA8F6BDCB8008846717171A4A4A443434393939362626269696968686867
        67677070709999994040407474746D6D6D656565A3A3A3434343397FAC6EAEDB
        00863A69D8B100C18300CD9400D8A100E2B000E2AD6BDEBD007F3B00E8B600DB
        A600C98E83E2C3008745717171A4A4A4424242A0A0A06060606666666C6C6C71
        7171717171A4A4A43F3F3F7474746D6D6D646464B2B2B2434343397FAC6EADDB
        00863981DFBE00C08100CC9200D69F00E1AE00E0AA82E4C7007E3900E8B500DB
        A400C88C99E8CF008744717171A4A4A4424242AFAFAF6060606666666B6B6B70
        7070707070B2B2B23E3E3E7474746D6D6D646464C0C0C0434343397FAC6DADDB
        00863897E5C900BF8000CB9100D69F00E0AD00DFA998EAD1007E3845F3CE00DA
        A100C789ADEEDA008743717171A3A3A3424242BDBDBD5F5F5F6565656B6B6B70
        70706F6F6FC0C0C03E3E3E9B9B9B6D6D6D636363CDCDCD434343397FAC6DADDA
        008537ADECD600BD7D00CA8E00D59C00DFAB00DEA7AFEFDC007F39CCEFE3D6F4
        EBD1F4EAD4FBF1008743717171A3A3A3424242CCCCCC5E5E5E6565656A6A6A6F
        6F6F6F6F6FCECECE3F3F3FDDDDDDE5E5E5E2E2E2E7E7E7434343397FAC6DAEDA
        008637E9FEF778E1C054DFB821DCAD21E6BA1BE5B7DAFBF300833E008C4C2BA0
        6A36A472C0E4D4008946717171A3A3A3424242F3F3F3ACACAC9999997E7E7E83
        83837F7F7FEAEAEA4141414545456464646C6C6CD2D2D2444444397FAC6FB0DB
        008A3A2EAF7761CBA595E7CFBCFDF0BBFFF5BBFFF58AE0C5008845FF00FFCEE9
        DCA8D7C143A978008D4C717171A4A4A44444446D6D6D959595BDBDBDDCDCDCDC
        DCDCDCDCDCB4B4B4434343FF00FFDBDBDBBFBFBF74747446464677A8C63D7FB1
        76AFBB99D1B432A06A008744008642008541008642078C4B77C2A0FF00FFFF00
        FFFF00FFFF00FFFF00FF9D9D9D767676979797B5B5B568686843434342424242
        42424242424747479C9C9CFF00FFFF00FFFF00FFFF00FFFF00FF}
      NumGlyphs = 2
      ParentFont = False
      OnClick = BSetStartAddressClick
    end
    object BGetStartAddressFromMap: TSpeedButton
      Left = 178
      Top = 96
      Width = 150
      Height = 25
      Hint = #1055#1086#1080#1089#1082' '#1087#1086' '#1082#1072#1088#1090#1077
      AllowAllUp = True
      GroupIndex = 1
      Caption = #1053#1072#1081#1090#1080' '#1087#1086' '#1082#1072#1088#1090#1077
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      Glyph.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000000000000000000000000000000000000FF00FF005F6A
        6100636F6400646F6400646F6500646F6500646F6600646F6600646F6600646F
        6600646F6600646F6700646F670064706700616C63007F8580005F6A6000EBF5
        EC00D4EDD700D4EED700D4EED800D5EED800D5EED900D5EED900D6EFDA00D6EF
        DA00D4E2EC00CFE5D600D5EDD900D8EFDC00D5EDD900616C6300626E6400EEF8
        EF00A4DBBC008CCAA60098D5B10086C9A10074ADC0005687C60083C2B60077AC
        AF006970EB0081C5A3008CD0A60085CAA000D2E9D700646F6700616E6400ECF7
        EE0096DBAF007FC99A008FD8AA007AC897008AD6A9005FA2AC005588D700485D
        E00079A8C9007FC9990087D0A00080CA9A00D6EEDA00646F6600616E6300F7FB
        F8009BDEC40073C3930080CF9F006FC18F007ECE9D006EBF8C0070A4BB004858
        DA004C7CD100518DB60073BE9D0074C38F00D7EFDA00646F6600616E6300F8FC
        F900BCFBFB009DE7DF0093E1BB0077C9970089DAA90075C69800717CF3006BAE
        AD0082E1DD0058A7E100568CE600619FBD00D0E9DB00646F6600606D6300F8FC
        F800A4EBED008DDFDF0097EBEB0072CFB70074CA99005683B0006078D70051C3
        B60090D5CD0050C6C6007ED0DD006499DB00CAE4DC00646F6600606D6200F8FC
        F800AFFAFA0094EBEB00A2F9FA008AEAEB0095EDF300595FEB0087D9E6005CD2
        D100DCDBC900ACD4CD0097DAD40077D1CE00D4EFDB00646F6500606D6100F8FC
        F8009FF1F10081DDDF008AEAEB0075DEDE006591EE00557EE20068DCDD0077BF
        B700E5C8B200DABAA300DEBCA600D9B79F00D6EDD800646F64005F6D6100F8FC
        F800A6F9F9008BE9EA0099F8FA0078D3EB00656BFF0077DCEA0070DFDE00C1C5
        B500F1D1BA00E5C1A900EAC7AE00E4BDA100D6EDD700636F64005F6D6100F8FC
        F80090EAEA0078DDDE0081E9EA004E6BE200639DEE005ED7D7005BCBC900D4B0
        9700E2BA9F00D6AC8F00DBB09100D4A78600D6EDD700636E64005F6D6100F7FC
        F8009FF9F90085E9EA0084D3FA00525AF00087F2F70060DAD70098D5CA00E0B7
        9C00EDC7A900E0B39400E6B89800DEAE8C00D7ECD600636E64005F6D6000F7FC
        F8008AEAEA0072DDDE005665F000569FDF0073E8E80046C1C100BBBBA500D0A4
        8300DEB08E00D19E7A00D6A27A00CF987100D7EBD500626E64005F6D6000F7FC
        F8009DF9F9006CB4ED006271FE0080E7E9008CF4F40052CDCD00ECC2A400DDAD
        8A00EBBA9700DDA78000E2AB8300DAA07500D9EAD400616E64005C6A5D00FBFC
        FB00FCFEFC00F7FCF800F7FCF800F7FCF800F8FCF800F7FCF900F8FCF800F8FC
        F800ECF7EE00EDF7EE00EFF6ED00EEF4EC00EBF4EB005E6A5F00788079005C6A
        5D005F6D60005F6D60005F6D60005F6D60005F6D61005F6D61005F6D61005F6D
        6100606D6100606D6200606D6200606D63005E6A5F0079807A00}
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      OnClick = BGetStartAddressFromMapClick
    end
    object CBStartHouse: TSearchComboBox
      Left = 63
      Top = 60
      Width = 266
      Height = 22
      OnChange = CBStartHouseChange
      Anchors = [akLeft, akTop, akRight]
      AddNewText = False
      AutoFill = False
      CaseSensitive = False
      Color = clWhite
      DropDownBox.Color = clWhite
      DropDownBox.CycleList = True
      DropDownBox.Font.Charset = DEFAULT_CHARSET
      DropDownBox.Font.Color = clWindowText
      DropDownBox.Font.Height = -11
      DropDownBox.Font.Name = 'Arial'
      DropDownBox.Font.Style = []
      DropDownBox.ItemHeight = 14
      DropDownBox.ShowItems = 8
      MaxLength = 0
      Mode = mFilter
      ModeHotKeys = True
      ParentColor = False
      ShowMode = False
      Sorted = False
      Style = sDropDownList
      TabOrder = 1
      TabStop = True
      UserButtons = <>
    end
    object CBStartStreet: TSearchComboBox
      Left = 63
      Top = 27
      Width = 266
      Height = 22
      OnChange = CBStartStreetChange
      Anchors = [akLeft, akTop, akRight]
      AddNewText = False
      AutoFill = False
      CaseSensitive = False
      Color = clWhite
      DropDownBox.Color = clWhite
      DropDownBox.CycleList = True
      DropDownBox.Font.Charset = DEFAULT_CHARSET
      DropDownBox.Font.Color = clWindowText
      DropDownBox.Font.Height = -11
      DropDownBox.Font.Name = 'Arial'
      DropDownBox.Font.Style = []
      DropDownBox.ItemHeight = 14
      DropDownBox.ShowItems = 8
      MaxLength = 0
      Mode = mFilter
      ModeHotKeys = True
      ParentColor = False
      ShowMode = False
      Sorted = False
      Style = sDropDownList
      TabOrder = 0
      TabStop = True
      UserButtons = <>
    end
  end
  object GBEnd: TGroupBox
    Left = 0
    Top = 145
    Width = 346
    Height = 145
    Align = alTop
    Caption = ' '#1050#1086#1085#1077#1094' '#1087#1091#1090#1080': '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    DesignSize = (
      346
      145)
    object LEndStreet: TLabel
      Left = 14
      Top = 29
      Width = 36
      Height = 14
      Caption = #1059#1083#1080#1094#1072': '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object LEndHouse: TLabel
      Left = 14
      Top = 62
      Width = 28
      Height = 14
      Caption = #1044#1086#1084': '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object BSetEndAddress: TSpeedButton
      Left = 16
      Top = 96
      Width = 150
      Height = 25
      Caption = #1042#1099#1073#1088#1072#1090#1100' '#1072#1076#1088#1077#1089' '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      Glyph.Data = {
        36060000424D3606000000000000360000002800000020000000100000000100
        18000000000000060000C40E0000C40E00000000000000000000FF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF3A81AC3980AB
        3A81ACFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FF737373727272737373FF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF387FAB6BB2D4
        3980ABFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FF7171719F9F9F727272FF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF387FAA69B0D3
        387FAAFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FF7171719E9E9E717171FF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF387FAA69B1D2
        3882A9FF00FFFF00FFFF00FFFF00FFFF00FFF0F0F0CDCDCDCCCCCCCCCCCCCCCC
        CCDEDEDEF5F5F5FF00FF7171719D9D9D707070FF00FFFF00FFFF00FFFF00FFFF
        00FFF0F0F0CDCDCDCCCCCCCCCCCCCCCCCCDEDEDEF5F5F5FF00FF387FAA6AB3D2
        3987A7DFDFDFF2F2F2FF00FFFF00FFFF00FFA0A4CA3343C42E3EC22C3BC02B3A
        BF646CC4ABAFCAE0E0E07171719E9E9E707070DFDFDFF2F2F2FF00FFFF00FFFF
        00FFB5B5B57B7B7B787878767676757575949494BABABAE000E03880AA66ADD1
        2C37C36A73C7A2A6C9CCCCCCCCCCCCCCCCCC2F3FC32E3EC22B3CC2718CFF7893
        FF4E68EC3147D46E77C77171719B9B9B777777989898B5B5B5CCCCCCCCCCCCCC
        CCCC797979787878767676B8B8B8BBBBBB9D9D9D8282829A489A3880AA6AB6D0
        2A33C22A42D83244CC2C3BBF2B3ABF2B3ABF2A3ABF3042CC2837BD6D88FF738E
        FE506CF47A8DF52B3BC07171719D9D9D7676768181817F7F7F75757575757575
        75757474747E7E7E727272B6B6B6B8B8B8A2A2A2B7B7B77575753880AA69B6CF
        2731C07285F23E5CEF5774F7657FF76880F25D78F88394F92533BB6C87FE728C
        FD4D69F28B9BF52938BF7171719C9C9C737373B2B2B2969696A7A7A7AEAEAEAD
        ADADAAAAAABEBEBE707070B5B5B5B7B7B79F9F9FC0C0C07474743880AA69B6CF
        252FBF8494F33B58EC546FF36A85FB7A95FF5C77F890A0F72231B96B86FE718C
        FD4C68F29DAAF72737BF7171719C9C9C727272BBBBBB939393A3A3A3B2B2B2BC
        BCBCAAAAAAC3C3C36D6D6DB4B4B4B7B7B79F9F9FCACACA7373733880AA69B6CF
        232DBF97A3F53956EC526EF26883F97892FE5974F6A1AEF91F2FB96884FE6F8A
        FD4966F2AEB9F92636BF7171719C9C9C717171C6C6C6929292A2A2A2B0B0B0BB
        BBBBA7A7A7CDCDCD6C6C6CB3B3B3B6B6B69D9D9DD3D3D37272723880AA69B5CF
        222CBFA9B4F73654EB526DF26883F97892FE5773F6B2BDFA1B2BB997ABFF6884
        FE4260F2BEC7FA2434BF7171719C9C9C707070D0D0D0909090A2A2A2B0B0B0BB
        BBBBA6A6A6D6D6D66A6A6ACBCBCBB3B3B39A9A9ADCDCDC7171713880AA68B5CF
        202BBFBAC3FA314FEB4C69F16480F9748FFE5470F6C2CBFC1A2BBBDADEF7E4E8
        FBDEE3FCDEE5FF2333BF7171719B9B9B6F6F6FDADADA8E8E8E9E9E9EAEAEAEB9
        B9B9A5A5A5DFDFDF6A6A6AE8E8E8EFEFEFEDEDEDEEEEEE7171713880AA69B5D0
        212BBFEBEEFF9AAAF78DA0F77D93FB8AA1FF7188F9E3E9FF1F2FBD2E3EC3515E
        CC5A67CFCBD0F12737C17171719C9C9C707070F5F5F5C8C8C8C2C2C2BCBCBCC4
        C4C4B5B5B5F1F1F16E6E6E7878788E8E8E949494DEDEDE7474743980AB6AB7D2
        262FC35665D98492E9B7C2F8DEE6FFE4EBFFDAE2FFA8B3F42536C0FF00FFD6D9
        F3B6BBEA606CD12D3DC37272729E9E9E747474979797B6B6B6D7D7D7EEEEEEF1
        F1F1ECECECCECECE727272FF00FFE4E4E4D0D0D098989878787876A8C53984AA
        7EA3CDAAAFE7515ECD2435BF2233BE2232BE2333BF2D3CC28E98DEFF00FFFF00
        FFFF00FFFF00FFFF00FF9D9D9D717171A5A5A5C8C8C88E8E8E71717170707070
        7070717171777777B6B6B6FF00FFFF00FFFF00FFFF00FFFF00FF}
      NumGlyphs = 2
      ParentFont = False
      OnClick = BSetEndAddressClick
    end
    object BGetEndAddressFromMap: TSpeedButton
      Left = 178
      Top = 96
      Width = 150
      Height = 25
      Hint = #1055#1086#1080#1089#1082' '#1087#1086' '#1082#1072#1088#1090#1077
      AllowAllUp = True
      GroupIndex = 1
      Caption = #1053#1072#1081#1090#1080' '#1087#1086' '#1082#1072#1088#1090#1077
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      Glyph.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000000000000000000000000000000000000FF00FF005F6A
        6100636F6400646F6400646F6500646F6500646F6600646F6600646F6600646F
        6600646F6600646F6700646F670064706700616C63007F8580005F6A6000EBF5
        EC00D4EDD700D4EED700D4EED800D5EED800D5EED900D5EED900D6EFDA00D6EF
        DA00D4E2EC00CFE5D600D5EDD900D8EFDC00D5EDD900616C6300626E6400EEF8
        EF00A4DBBC008CCAA60098D5B10086C9A10074ADC0005687C60083C2B60077AC
        AF006970EB0081C5A3008CD0A60085CAA000D2E9D700646F6700616E6400ECF7
        EE0096DBAF007FC99A008FD8AA007AC897008AD6A9005FA2AC005588D700485D
        E00079A8C9007FC9990087D0A00080CA9A00D6EEDA00646F6600616E6300F7FB
        F8009BDEC40073C3930080CF9F006FC18F007ECE9D006EBF8C0070A4BB004858
        DA004C7CD100518DB60073BE9D0074C38F00D7EFDA00646F6600616E6300F8FC
        F900BCFBFB009DE7DF0093E1BB0077C9970089DAA90075C69800717CF3006BAE
        AD0082E1DD0058A7E100568CE600619FBD00D0E9DB00646F6600606D6300F8FC
        F800A4EBED008DDFDF0097EBEB0072CFB70074CA99005683B0006078D70051C3
        B60090D5CD0050C6C6007ED0DD006499DB00CAE4DC00646F6600606D6200F8FC
        F800AFFAFA0094EBEB00A2F9FA008AEAEB0095EDF300595FEB0087D9E6005CD2
        D100DCDBC900ACD4CD0097DAD40077D1CE00D4EFDB00646F6500606D6100F8FC
        F8009FF1F10081DDDF008AEAEB0075DEDE006591EE00557EE20068DCDD0077BF
        B700E5C8B200DABAA300DEBCA600D9B79F00D6EDD800646F64005F6D6100F8FC
        F800A6F9F9008BE9EA0099F8FA0078D3EB00656BFF0077DCEA0070DFDE00C1C5
        B500F1D1BA00E5C1A900EAC7AE00E4BDA100D6EDD700636F64005F6D6100F8FC
        F80090EAEA0078DDDE0081E9EA004E6BE200639DEE005ED7D7005BCBC900D4B0
        9700E2BA9F00D6AC8F00DBB09100D4A78600D6EDD700636E64005F6D6100F7FC
        F8009FF9F90085E9EA0084D3FA00525AF00087F2F70060DAD70098D5CA00E0B7
        9C00EDC7A900E0B39400E6B89800DEAE8C00D7ECD600636E64005F6D6000F7FC
        F8008AEAEA0072DDDE005665F000569FDF0073E8E80046C1C100BBBBA500D0A4
        8300DEB08E00D19E7A00D6A27A00CF987100D7EBD500626E64005F6D6000F7FC
        F8009DF9F9006CB4ED006271FE0080E7E9008CF4F40052CDCD00ECC2A400DDAD
        8A00EBBA9700DDA78000E2AB8300DAA07500D9EAD400616E64005C6A5D00FBFC
        FB00FCFEFC00F7FCF800F7FCF800F7FCF800F8FCF800F7FCF900F8FCF800F8FC
        F800ECF7EE00EDF7EE00EFF6ED00EEF4EC00EBF4EB005E6A5F00788079005C6A
        5D005F6D60005F6D60005F6D60005F6D60005F6D61005F6D61005F6D61005F6D
        6100606D6100606D6200606D6200606D63005E6A5F0079807A00}
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      OnClick = BGetEndAddressFromMapClick
    end
    object CBEndHouse: TSearchComboBox
      Left = 63
      Top = 60
      Width = 266
      Height = 22
      OnChange = CBEndHouseChange
      Anchors = [akLeft, akTop, akRight]
      AddNewText = False
      AutoFill = False
      CaseSensitive = False
      Color = clWhite
      DropDownBox.Color = clWhite
      DropDownBox.CycleList = True
      DropDownBox.Font.Charset = DEFAULT_CHARSET
      DropDownBox.Font.Color = clWindowText
      DropDownBox.Font.Height = -11
      DropDownBox.Font.Name = 'Arial'
      DropDownBox.Font.Style = []
      DropDownBox.ItemHeight = 14
      DropDownBox.ShowItems = 8
      MaxLength = 0
      Mode = mFilter
      ModeHotKeys = True
      ParentColor = False
      ShowMode = False
      Sorted = False
      Style = sDropDownList
      TabOrder = 1
      TabStop = True
      UserButtons = <>
    end
    object CBEndStreet: TSearchComboBox
      Left = 63
      Top = 27
      Width = 266
      Height = 22
      OnChange = CBEndStreetChange
      Anchors = [akLeft, akTop, akRight]
      AddNewText = False
      AutoFill = False
      CaseSensitive = False
      Color = clWhite
      DropDownBox.Color = clWhite
      DropDownBox.CycleList = True
      DropDownBox.Font.Charset = DEFAULT_CHARSET
      DropDownBox.Font.Color = clWindowText
      DropDownBox.Font.Height = -11
      DropDownBox.Font.Name = 'Arial'
      DropDownBox.Font.Style = []
      DropDownBox.ItemHeight = 14
      DropDownBox.ShowItems = 8
      MaxLength = 0
      Mode = mFilter
      ModeHotKeys = True
      ParentColor = False
      ShowMode = False
      Sorted = False
      Style = sDropDownList
      TabOrder = 0
      TabStop = True
      UserButtons = <>
    end
  end
  object PToolbar: TPanel
    Left = 0
    Top = 704
    Width = 346
    Height = 30
    Align = alBottom
    BevelOuter = bvNone
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    object BCalculate: TBitBtn
      Left = 73
      Top = 4
      Width = 200
      Height = 25
      Caption = #1055#1086#1089#1090#1088#1086#1080#1090#1100' '#1087#1091#1090#1100
      TabOrder = 0
      OnClick = BCalculateClick
      Glyph.Data = {
        36060000424D3606000000000000360400002800000020000000100000000100
        080000000000000200000000000000000000000100000000000000000000FFFF
        FF001616AD001414AB001B1CB1008887BB008F8BB500908EB9009593BA00F5EB
        D700D8C39B00DCCEB300E4D5B700E8E0D500CEBDA100D3C8B600DBCFB900ECEC
        EF00E0DEDC00DEDAD400E8EAEE00B8BABD00CFBDA000D2B47F00C4C5C9003EF6
        AE0038F5AA0019DB90001EE698001FE094001EE2930021E5950026D28A0021D1
        870022D68A004CEEA60046E19C003BD6920023CE840023D88A0044F1A6003ADA
        94001DCB810020CA800020CB7F002BCF870021C97F0023CA800025C97F0024C8
        7E0035DF920022C87C0026C87D0025C77C0027C47B0028C47A0028C478002CDF
        8B0027C87B0028C4790029C1780028C077002ABF74002BBF74002ABE73002DC4
        750030CB780028BC72002ABF73002BBD72002CBC70002DBA6F002EB96D002EB8
        6C002FB86C00C0C0C00000000000000000000000000000000000000000000000
        000035DF920022C87C0026C87D0025C77C0027C47B0028C47A0028C478002CD2
        8100000000000000000000000000000000000000000000000000000000000000
        00003BE69A002BCF870021C97F0023CA800025C97F0024C87E0027CD7F000000
        0000000000000000000000000000000000000000000000000000000000000000
        00003BE69A002BCF870021C97F0023CA800025C97F0024C87E0027CD7F000000
        0000000000000000000000000000000000000000000000000000000000000000
        00000000000044F1A6003ADA94001DCB810020CA800020CB7F0023CB81000000
        0000000000000000000000000000000000000000000000000000000000000000
        00000000000044F1A6003ADA94001DCB810020CA800020CB7F0023CB81000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000027B679004CEEA60046E19C003BD6920023CE840023D8
        8A0024D98A000000000000000000000000000000000000000000000000000000
        0000000000000000000027B679004CEEA60046E19C003BD6920023CE840023D8
        8A0024D98A000000000000000000000000000000000000000000000000000000
        000000000000000000000000000000000000000000000000000026D28A0021D1
        870022D68A0025E8950000000000000000000000000000000000000000000000
        000000000000000000000000000000000000000000000000000026D28A0021D1
        870022D68A0025E8950000000000000000000000000000000000000000000000
        000000000000000000001DDF94001EE698001FE094001EE2930021E595001CC5
        8000179963000000000000000000000000000000000000000000000000000000
        000000000000000000001DDF94001EE698001FE094001EE2930021E595001CC5
        80001799630000000000000000000000000000000000000000004B4B4B4B4344
        45464748494A4B4B4B4B4B4B4B4B06060606060606064B4B4B4B4B4B393A3B3C
        3D3E3F4041424B4B4B4B4B4B060606060606060606064B4B4B4B4B4B32333435
        3637384B4B4B4B4B4B4B4B4B060606060606064B4B4B4B4B4B4B4B4B4B2D2E2F
        30314B4B4B4B4B4B4B4B4B4B4B06060606064B4B4B4B4B4B4B4B4B4B4B28292A
        2B2C4B4B4B4B4B4B4B4B4B4B4B08060606064B4B4B4B4B4B4B4B4B4B4B4B4B23
        242526274B4B4B4B4B4B4B4B4B4B4B08060606064B4B4B4B4B4B4B4B4B4B4B4B
        4B4B2021224B4B4B4B4B4B4B4B4B4B4B4B4B0606064B4B4B4B4B4B4B4B4B4B1C
        1D1E1F4B4B4B4B4B4B4B4B4B4B4B4B060606064B4B4B4B4B4B4B4B4B4B4B1A1B
        4B4B4B4B4B4B4B4B4B4B4B4B4B4B08064B4B4B4B4B4B4B4B4B4B4B4B4B4B4B19
        4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B084B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B
        4B151617184B4B4B4B4B4B4B4B4B4B4B4B151515184B4B4B4B4B4B4B4B4B4B4B
        4B111213144B4B4B4B4B4B4B4B4B4B4B4B111213114B4B4B4B4B4B4B4B4B4B4B
        4B0D0E0F104B4B4B4B4B4B4B4B4B4B4B4B121518184B4B4B4B4B4B4B4B4B4B4B
        4B090A0B0C4B4B4B4B4B4B4B4B4B4B4B4B141518134B4B4B4B4B4B4B4B4B4B4B
        4B050607084B4B4B4B4B4B4B4B4B4B4B4B060608084B4B4B4B4B4B4B4B4B4B4B
        4B4B0203044B4B4B4B4B4B4B4B4B4B4B4B4BEAEAEA4B4B4B4B4B}
      NumGlyphs = 2
    end
  end
  object GBResult: TGroupBox
    Left = 0
    Top = 290
    Width = 346
    Height = 414
    Align = alClient
    Caption = ' '#1055#1086#1089#1090#1088#1086#1077#1085#1085#1099#1081' '#1087#1091#1090#1100' '
    TabOrder = 4
    object PResultRoutesType: TPanel
      Left = 2
      Top = 16
      Width = 342
      Height = 30
      Align = alTop
      BevelOuter = bvNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      object RBBranches: TRadioButton
        Left = 192
        Top = 8
        Width = 73
        Height = 17
        Caption = #1055#1086' '#1076#1091#1075#1072#1084
        TabOrder = 0
        OnClick = RBBranchesClick
      end
      object RBStreets: TRadioButton
        Left = 72
        Top = 8
        Width = 81
        Height = 17
        Caption = #1055#1086' '#1091#1083#1080#1094#1072#1084
        Checked = True
        TabOrder = 1
        TabStop = True
        OnClick = RBBranchesClick
      end
    end
    object VTResult: TVirtualStringTree
      Left = 2
      Top = 46
      Width = 342
      Height = 366
      Align = alClient
      Header.AutoSizeIndex = 0
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clWindowText
      Header.Font.Height = -11
      Header.Font.Name = 'MS Sans Serif'
      Header.Font.Style = []
      Header.Options = [hoColumnResize, hoDrag, hoVisible]
      TabOrder = 1
      OnChange = VTResultChange
      OnClick = VTResultClick
      OnGetText = VTResultGetText
      Columns = <
        item
          Position = 0
          Width = 200
        end
        item
          Position = 1
          Width = 132
        end>
    end
  end
end
