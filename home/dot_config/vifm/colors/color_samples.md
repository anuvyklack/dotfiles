# Available group-name values:

* Win          : color of all windows (views, dialogs, menus) and default
               color for their content (e.g. regular files in views)
AuxWin       : color of auxiliary areas of windows
OtherWin     : color of inactive pane
* Border       : color of vertical parts of the border
* TabLine      : tab line color (for |vifm-'tabscope'| set to "global")
* TabLineSel   : color of the tip of selected tab (regardless of
               |vifm-'tabscope'|)
* TopLine      : top line color of the other pane
* TopLineSel   : top line color of the current pane
* CmdLine      : the command line/status bar color
* ErrorMsg     : color of error messages in the status bar
* StatusLine   : color of the line above the status bar
JobLine      : color of job line that appears above the status line
* WildMenu     : color of the wild menu items
* SuggestBox   : color of key suggestion box
* CurrLine     : line at cursor position in active view
* OtherLine    : line at cursor position in inactive view
OddLine      : color of every second entry line in a pane
LineNr       : line number column of views
* Selected     : color of selected files
* Directory    : color of directories
* Executable   : color of executable files
* Link         : color of symbolic links in the views
* BrokenLink   : color of broken symbolic links
* HardLink     : color of regular files with more than one hard link
* Socket       : color of sockets
* Device       : color of block and character devices
* Fifo         : color of fifo pipes
CmpMismatch  : color of mismatched files in side-by-side comparison by paths
User1..User9 : 9 colors which can be used via %* |vifm-'statusline'| macro


# Available style values (some of them can be combined):

bold
underline
reverse / inverse
standout
italic
combine : add attributes of current group to attributes of the parent in
          group hierarchy (see below) instead of replacing them
none


# The hierarchy of highlight groups

You need this information for using transparency:

    JobLine
    SuggestBox
    StatusLine
      WildMenu
      User1..User9
    Border
    CmdLine
      ErrorMsg
    Win
      OtherWin
        AuxWin
          OddLine
            File name specific highlights
              Directory
              Link
              BrokenLink
              HardLink
              Socket
              Device
              Fifo
              Executable
                Selected
                  CurrLine
                    LineNr (in active pane)
                  OtherLine
                    LineNr (in inactive pane)
    TopLine
      TopLineSel
        TabLineSel (for pane tabs)
          User1..User9
    TabLine
      TabLineSel
        User1..User9

"none" means default terminal color for highlight groups at the first level
of the hierarchy and transparency for all others.

-------------------------------------------------------------------------------

    #E9B143 #B57919 #D65D0E #A1C42C #BEA8CC #9EB7CD #93B29D #FF7F50 #F4AAB7 #FAF1D0
    #E3A84E #F19D00 #D79921 #D3D70E #8C56B6 #68CACD #3F5950 #FB9A3B #E499C4 #FFD998
    #E09952 #E2AC02 #E9B143 #879301 #7E247E #198496 #146252 #DC6B31 #D188BF
    #F7C96E         #FABD2F #666723 #682F5A #114471 #06483A #E1541F         #D1BA51
    #AB9A3F #C5894D #DBBF74
    #CCA44C #9B4F11                         #58B997
                                            #26A387
    #AB9A3F                                 #268BA3
    #CCA44C                                 #34A2BD
                                            #38BBDC
                                            #3CA2A8

-------------------------------------------------------------------------------

    Sienna   Salmon
    #E97451  #FA8072  #D65D0E  #F07818
    #D05E39  #FA6857  #D46217  #ED7304
    #B84A2A  #FA5039  #E06D22  #DB7635
    #9D3314  #FA351F  #EB764A  #C96C2E
                               #C46425
                               #C96A2C
                               #BD6B35
                               #CF7F4A

    #B47109  #B4730E  #B57614

-------------------------------------------------------------------------------
# Gruvbox Material

             #D4BE98
             #DDC7A1  #E2CCA9  #EBDBB2
             #EA6962  #F2594B  #FB4934   #C14A4A  #AF2528  #9D0006  #654735  #F9F5D7  #1D2021
    #D65D0E  #E78A4E  #F28534  #FE8019   #C35E0A  #B94C07  #AF3A03  #514036  #FBF1C7  #282828
    #D79921  #D8A657  #E9B143  #FABD2F   #B47109  #B4730E  #B57614  #4F3829  #F2E5BC  #32302F
    #689D6A  #89B482  #8BBA7F  #8EC07C   #6C782E  #72761E  #79740E           #EBDBB2  #3C3836
    #458588  #7DAEA3  #80AA9E  #83A598   #4C7A5D  #477A5B  #427B58           #D5C4A1  #504945
    #B16286                    #D3869B   #45707A  #266B79  #076678           #BDAE93  #665C54
             #EA6962  #DB4740  #CC241D   #945E80  #924F79  #8F3F71           #A89984  #7C6F64
    #98971A  #A9B665  #B0B846  #B8BB26   #AE5858                             #928374
    #AB9A3F                              #6F8352
                                         #A96B2C

    #45403D  #46413E  #5A524C   #F5EDCA  #F4E8BE  #EDDEB5
    #3A3735                     #F3EAC7
    #5B534D                                       #E6D5AE
    #32361A  #34381B  #3D4220   #EEE0B7  #E5D5AD  #DAC9A5
    #333E34  #3B4439  #424A3E   #DDCCAB
    #3C1F1E  #402120  #472322   #E4EDC8  #E6EABC  #DFE1B4
    #442E2D  #4C3432  #543937   #DDE5C2  #DEE2B6  #D7D9AE
    #0D3138  #0E363E  #0F3A42   #F8E4C9  #F9E0BB  #F7D9B9
    #2E3B3B  #374141  #404946   #F0DDC3  #F1D9B5  #EFD2B3
    #473C29  #4F422E  #574833   #E0E9D3  #E2E6C7  #DBDDBF
                                #D9E1CC  #DADEC0  #D3D5B8
                                #F9EABF  #FAE7B3  #F3DEAA

-------------------------------------------------------------------------------
# TVA

      #E9C066
      #FDB843
      #EFA72F

      #CA8B46  #F28658  #FFFBDD           #CEC6AF  #E0E0C6
      #D78D20  #FC7548  #D5D5A9  #66F5FF  #DBEACD  #C0C6AA  #FCEBB6
      #F69D37  #D05E3A  #94BCAA           #BDD2AC  #ABB7A3  #F0E1B6 #FAE397 #FBCC74 #D9A26A
      #F18F36  #C04B00  #4A8E78  #9FCEDE  #6D8271  #687C70          #F4D288
    * #F28534  #B3452C  #427168  #30B0D7                    #E7C5AA
      #F0851B  #AD4335  #426159  #2BACD3  #9B9A7C  #A8D389  #EF947F
      #E98000  #A8390E  #415F57  #2A9FCB  #9B9777
      #E47700  #AC281C  #3E5B56           #909A99  #78BFA8  #935C41
      #CC732F  #812D22  #415C53  #4293C9                    #834630
                        #4C5C5C           #928374           #5E412F
               #BF2E1B           #5ca3f8                    #594637

-------------------------------------------------------------------------------
# Blasphemous

    #4C3F42  #BDAD7E  #E55F3D
    #333B40  #AF8F73  #EF6910
    #866647  #7D6962  #F1A20E
    #8F7062  #4E4D4A  #EEAD0E
    #6A706A  #4E4E53  #E5C07B
    #DDAA79
    
    #C24278  #C678DD
    
    #EE8585  #2C2828  #F3F27B  #79688F  #B9F4D0  #4D5426  #76685A  #58B997
    #B44962  #37322E  #F9D33D  #6F578D  #58D6B7  #868E65  #826950  #26A387
    #917F8B  #1C2731  #EAB734  #4F366A  #0F878A  #AB9A3F  #8C7755  #268BA3
    #A8C0AF  #120B12  #E19E2C                    #8B8560  #9F9160  #34A2BD
    #5D858D  #292121  #DB8B2A  #4F4EAD  #139080  #8E6645  #E2C48F  #38BBDC
    #98AD9D  #421015  #E6A264  #454182  #096570  #7C4C3E  #C8AD83  #3CA2A8
    #E37D57  #535F60  #DE8B52  #504780  #4F7285  #794B34  #A08071
    #7D7B7B  #404A4A  #D6752A  #820F60  #7894AA  #613523
             #321524  #FF8A00  #854366  #84979D           #2047A9
                               #713965  #4A6D7F  #D1152B  #0960C1
             #0E0C09           #682B6D  #67849B  #9B1930  #188FD3
                               #56243E  #97A3B0  #893F34  #1B91D4
                               #5E3B60  #8792A0
                               #463B57  #9999A1  #404B6F
    #47AFE9
    #357592
    #3DA4D4

    #ABBC6C
    #969D3E

    #26A387  #3DA4D4
    #189F7F  #30BDFF
    #36B295  #48C5FF
    #58B997  #5CCBFF
    #5DC4AC

## Мать матерей

    #664C35
    #8E7246
    #8B6B45

    #E8B238
    #ED6D44
    #A23332
    #7C5A82

-------------------------------------------------------------------------------
# Melagne

            #C9B39C
            #998066
    
    #997733 #E0CCB8
    #856647 #B8997A
    #544D45 #E0C285
    
    #244224 #D194BD #94D194 #94D1D1 #F7C96E #EBB8AD #F7F2ED
    #242E42 #AD85AD #7AB87A #85ADAD #D99D26 #F7856E #F0E6DB
    #422438         #669966                 #D65C5C #EDE6DE
    #38332E #B87AB8 #339933 #94A8D1 #E09952 #B34D4D #D9E1F2
    #2A2622 #B34D90         #8CA6D9 #CC8033 #AD1F1F #EDDEE8
    #423324         #669999 #5973A6         #6B2E2E #D9F2D9
                    #366363 #4D6EB3

-------------------------------------------------------------------------------
# Mellow

    Light    Dark
    -------  -------
    #0F0908
    #AF0032
    #4C6E25  #577E2A
    #A67458  #BF9169
    #573E55  #896186
    #66292F
    #BF472C
    #E0CCAE  #D3C1A6
    #3D241F
    #FF7477
    #84BF40
    #F5BB89
    #8A7B85  #9F939B
    #8A4B53
    #D47D49
    #F2DDBC  #ECCD9D

-------------------------------------------------------------------------------
# Solorized

    SOLARIZED HEX     HEX
    --------- ------- --------
    BASE03    #002B36  #1C1C1C
    BASE02    #073642  #262626
    BASE01    #586E75  #4E4E4E
    BASE00    #657B83  #585858
    BASE0     #839496  #808080
    BASE1     #93A1A1  #8A8A8A
    BASE2     #EEE8D5  #D7D7AF
    BASE3     #FDF6E3  #FFFFD7
    YELLOW    #B58900  #AF8700
    ORANGE    #CB4B16  #D75F00
    RED       #DC322F  #D70000
    MAGENTA   #D33682  #AF005F
    VIOLET    #6C71C4  #5F5FAF
    BLUE      #268BD2  #0087FF
    CYAN      #2AA198  #00AFAF
    GREEN     #859900  #5F8700
    
    #004164 #008080 #00FFFF

-------------------------------------------------------------------------------
# Material

             Lighter  Palenight  Darker   Ocean  
                                                 
    #263238  #FAFAFA   #292D3E   #212121  #0F111A
    #EEFFFF  #90A4AE   #A6ACCD   #EEFFFF  #8F93A2
    #65738E  #E7EAEC   #4E5579   #65737E  #80869E
    #546E7A  #90A4AE   #676E95   #545454  #464B5D
    #FFCC00  #272727   #343B51   #2C2C2C  #1F2233
    #2C3B41  #EBF4F3   #4E5579   #424242  #3B3F51
    #37474F  #B0BEC5   #3A3F58   #424242  #3B3F51
    #37474F  #CFD8DC   #1C1F2B   #171717  #0A0C12
    #1A2327  #ECF0F1
    #FFFFFF  #FFFFFF
    #000000  #000000
    #FF5370  #E53935
    #F78C6C  #F76D47
    #FFCB6B  #FFB62C
    #C3E88D  #91B859
    #89DDFF  #39ADB5
    #82AAFF  #6182B8
    #B2CCD6  #8796B0
    #C792EA  #7C4DFF
    #C17E70  #C17E70
    #F07178  #FF5370
    #BB80B3  #945EB8

-------------------------------------------------------------------------------
# OneDark

    Black         #282C34
    White         #ABB2BF
    Light Red     #E06C75
    Dark Red      #BE5046
    Green         #98C379
    Light Yellow  #E5C07B
    Dark Yellow   #D19A66
    Blue          #61AFEF
    Magenta       #C678DD
    Cyan          #56B6C2
    Gutter Grey   #4B5263
    Comment Grey  #5C6370


    background            #282C34
    foreground            #ABB2BF
    
    cursor                #D0D0D0
    selection_background  #444444
    
    # black
    color0                #282c34
    color8                #5c6370
    
    # red
    color1                #E06C75
    color9                #BE5046
    
    # green
    # color2                #86C38A
    color2                #98C379
    color10               #98C379
    # color10               #94F936
    
    # yellow
    color3                #D1A166
    # color3                #D19A66
    color11               #E5C07B
    # color11               #F5FFA7
    
    # blue
    color4                #61AFEF
    color12               #85BEFD
    
    # magenta
    color5                #C678DD
    color13               #DC5BFC
    # color13               #FF00FF
    # color5                #B9B5FC
    # color13               #B9B5FC
    
    # cyan
    color6                #56B6C2
    color14               #56B6C2
    
    # white
    color7                #ABB2BF
    color15               #FFFFFF
    
    selection_foreground  #161718

-------------------------------------------------------------------------------

    #304E67  #A5BEAB
    #52548F  #82BEC4
             #7BBEC9
    #574191

    #6CA6D6
    #07497B

    #89B01C


    "${rgb}226;157;157$rc"  # #e29d9d
    "${rgb}226;174;157$rc"  # #e2ae9d
    "${rgb}226;191;157$rc"  # #e2bf9d
    "${rgb}226;208;157$rc"  # #e2d09d
    "${rgb}226;226;157$rc"  # #e2e29d
    "${rgb}208;226;157$rc"  # #d0e29d
    "${rgb}191;226;157$rc"  # #bfe29d
    "${rgb}174;226;157$rc"  # #aee29d
    "${rgb}157;226;157$rc"  # #9de29d
    "${rgb}157;226;174$rc"  # #9de2ae
    "${rgb}157;226;191$rc"  # #9de2bf
    "${rgb}157;226;199$rc"  # #9de2c7
    "${rgb}157;226;208$rc"  # #9de2d0
    "${rgb}157;226;226$rc"  # #9de2e2
    "${rgb}157;208;226$rc"  # #9dd0e2
    "${rgb}157;191;226$rc"  # #9dbfe2
    "${rgb}157;174;226$rc"  # #9daee2
    "${rgb}157;157;226$rc"  # #9d9de2
    "${rgb}174;157;226$rc"  # #ae9de2
    "${rgb}191;157;226$rc"  # #bf9de2
    "${rgb}208;157;226$rc"  # #d09de2
    "${rgb}226;157;226$rc"  # #e29de2
    "${rgb}226;157;208$rc"  # #e29dd0
    "${rgb}226;157;191$rc"  # #e29dbf
    "${rgb}226;157;174$rc"  # #e29dae
    "${rgb}226;157;157$rc"  # #e29d9d

        black = { '#17191e', '#0e1013' , '#151820' , '#0c0e15' , '#191a1c' , '#101012' },
          bg0 = { '#282c34', '#1f2329' , '#242b38' , '#1a212e' , '#2c2d30' , '#232326' },
          bg1 = { '#31353f', '#282c34' , '#2d3343' , '#21283b' , '#35373b' , '#2c2d31' },
          bg2 = { '#393f4a', '#30363f' , '#343e4f' , '#283347' , '#3e4045' , '#35363b' },
          bg3 = { '#3b3f4c', '#323641' , '#363c51' , '#2a324a' , '#404247' , '#37383d' },
         bg_d = { '#21252b', '#181b20' , '#1e242e' , '#141b24' , '#242628' , '#1b1c1e' },
      bg_blue = { '#73b8f1', '#61afef' , '#6db9f7' , '#54b0fd' , '#79b7eb' , '#68aee8' },
    bg_yellow = { '#ebd09c', '#e8c88c' , '#f0d197' , '#f2cc81' , '#e6cfa1' , '#e2c792' },
           fg = { '#abb2bf', '#a0a8b7' , '#a5b0c5' , '#93a4c3' , '#b1b4b9' , '#a7aab0' },
       purple = { '#c678dd', '#bf68d9' , '#ca72e4' , '#c75ae8' , '#c27fd7' , '#bb70d2' },
        green = { '#98c379', '#8ebd6b' , '#97ca72' , '#8bcd5b' , '#99bc80' , '#8fb573' },
       orange = { '#d19a66', '#cc9057' , '#d99a5e' , '#dd9046' , '#c99a6e' , '#c49060' },
         blue = { '#61afef', '#4fa6ed' , '#5ab0f6' , '#41a7fc' , '#68aee8' , '#57a5e5' },
       yellow = { '#e5c07b', '#e2b86b' , '#ebc275' , '#efbd5d' , '#dfbe81' , '#dbb671' },
         cyan = { '#56b6c2', '#48b0bd' , '#4dbdcb' , '#34bfd0' , '#5fafb9' , '#51a8b3' },
          red = { '#e86671', '#e55561' , '#ef5f6b' , '#f65866' , '#e16d77' , '#de5d68' },
         grey = { '#5c6370', '#535965' , '#546178' , '#455574' , '#646568' , '#5a5b5e' },
   light_grey = { '#848b98', '#7a818e' , '#7d899f' , '#6c7d9c' , '#8b8d91' , '#818387' },
    dark_cyan = { '#2b6f77', '#266269' , '#25747d' , '#1b6a73' , '#316a71' , '#2b5d63' },
     dark_red = { '#993939', '#8b3434' , '#a13131' , '#992525' , '#914141' , '#833b3b' },
  dark_yellow = { '#93691d', '#835d1a' , '#9a6b16' , '#8f610d' , '#8c6724' , '#7c5c20' },
  dark_purple = { '#8a3fa0', '#7e3992' , '#8f36a9' , '#862aa1' , '#854897' , '#79428a' },
    diff_add =  { '#31392b', '#272e23' , '#303d27' , '#27341c' , '#32352f' , '#282b26' },
  diff_delete = { '#382b2c', '#2d2223' , '#3c2729' , '#331c1e' , '#342f2f' , '#2a2626' },
  diff_change = { '#1c3448', '#172a3a' , '#18344c' , '#102b40' , '#203444' , '#1a2a37' },
    diff_text = { '#2c5372', '#274964' , '#265478' , '#1c4a6e' , '#32526c' , '#2c485f' }

