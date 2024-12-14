    ALIGN_FREE_LBL  256, "ColorTbls"

ColorTbls
EnemyCol0
    .byte   CYAN|$6
    .byte   CYAN|$6
    .byte   CYAN|$8
    .byte   CYAN|$4
    .byte   CYAN|$a
    .byte   CYAN|$a
    .byte   0
    .byte   0
    .byte   CYAN|$a
    .byte   CYAN|$a
    .byte   CYAN|$c
    .byte   CYAN|$c
    .byte   CYAN|$e
    .byte   CYAN|$e
EnemyCol1
    .byte   MAUVE|$6+2
    .byte   MAUVE|$6+2
    .byte   MAUVE|$8+2
    .byte   MAUVE|$4+2
    .byte   MAUVE|$a+2
    .byte   MAUVE|$a+2
    .byte   0
    .byte   0
    .byte   MAUVE|$a+2
    .byte   MAUVE|$a+2
    .byte   MAUVE|$c+2
    .byte   MAUVE|$c+2
    .byte   MAUVE|$e+2*0
    .byte   MAUVE|$e+2*0
EnemyCol2
    .byte   ORANGE|$6-2
    .byte   ORANGE|$6-2
    .byte   ORANGE|$8-2
    .byte   ORANGE|$4-2
    .byte   ORANGE|$a-2
    .byte   ORANGE|$a-2
    .byte   0
    .byte   0
    .byte   ORANGE|$a-2
    .byte   ORANGE|$a-2
    .byte   ORANGE|$c-2
    .byte   ORANGE|$c-2
    .byte   ORANGE|$e-2
    .byte   ORANGE|$e-2
EnemyCol3
    .byte   BROWN|$6
    .byte   BROWN|$6
    .byte   BROWN|$8
    .byte   BROWN|$4
    .byte   BROWN|$a
    .byte   BROWN|$a
    .byte   0
    .byte   0
    .byte   BROWN|$a
    .byte   BROWN|$a
    .byte   BROWN|$c
    .byte   BROWN|$c
    .byte   BROWN|$e
    .byte   BROWN|$e
EnemyColDark
    .byte   BLUE|$6-2
    .byte   BLUE|$6-2
    .byte   BLUE|$8-2
    .byte   BLUE|$a-2
    .byte   BLUE|$a-2
    .byte   BLUE|$a-2
    .byte   0
    .byte   0
    .byte   BLUE|$a-2
    .byte   BLUE|$a-2
    .byte   BLUE|$c-2
    .byte   BLUE|$c-2
    .byte   BLUE|$e-2
    .byte   BLUE|$e-2
EnemyColBlink
    .byte   $8
    .byte   $8
    .byte   $a
    .byte   $a
    .byte   $a
    .byte   $a
    .byte   0
    .byte   0
    .byte   $c
    .byte   $c
    .byte   $c
    .byte   $c
    .byte   $e
    .byte   $e
EnemyEyesCol
    ds      GFX_H, $0e
CherryCol
    .byte   RED|$4
    .byte   RED|$6
    .byte   RED|$8
    .byte   RED|$8
    .byte   RED|$a
    .byte   RED|$a
    .byte   RED|$c
    .byte   RED|$c
    .byte   RED|$c
    .byte   GREEN|$4
    .byte   GREEN|$6
    .byte   GREEN|$6
    .byte   GREEN|$6
    .byte   GREEN|$8
StrawberryCol
    .byte   ORANGE|$4
    .byte   ORANGE|$6
    .byte   ORANGE|$8
    .byte   ORANGE|$8
    .byte   ORANGE|$8
    .byte   ORANGE|$8
    .byte   ORANGE|$8
    .byte   ORANGE|$8
    .byte   ORANGE|$8
    .byte   ORANGE|$a
    .byte   ORANGE|$a
    .byte   ORANGE|$a
    .byte   ORANGE|$c
    .byte   GREEN|$8
OrangeCol
    .byte   BROWN|$6
    .byte   BROWN|$8
    .byte   BROWN|$a
    .byte   BROWN|$a
    .byte   BROWN|$a
    .byte   BROWN|$a
    .byte   BROWN|$a
    .byte   BROWN|$c
    .byte   BROWN|$c
    .byte   BROWN|$c
    .byte   BROWN|$c
    .byte   BROWN|$e
    .byte   GREEN|$6
    .byte   GREEN|$8
AppleCol
    .byte   ORANGE|$4
    .byte   ORANGE|$6
    .byte   ORANGE|$8
    .byte   ORANGE|$8
    .byte   ORANGE|$8
    .byte   ORANGE|$8
    .byte   ORANGE|$8
    .byte   ORANGE|$8
    .byte   ORANGE|$a
    .byte   ORANGE|$a
    .byte   ORANGE|$a
    .byte   ORANGE|$c
    .byte   BEIGE|$6
    .byte   BEIGE|$6
MelonCol
    .byte   GREEN|$4
    .byte   GREEN|$6
    .byte   GREEN|$2    ; increase by 2?
    .byte   GREEN|$8
    .byte   GREEN|$8
    .byte   GREEN|$4
    .byte   0
    .byte   0
    .byte   GREEN|$a
    .byte   GREEN|$4
    .byte   GREEN|$a
    .byte   GREEN|$c
    .byte   GREEN|$6
    .byte   GREEN|$e
BananaCol
    .byte   YELLOW|$8
    .byte   YELLOW|$a
    .byte   YELLOW|$c
    .byte   YELLOW|$c
    .byte   YELLOW|$c
    .byte   YELLOW|$c
    .byte   0
    .byte   0
    .byte   YELLOW|$c
    .byte   YELLOW|$c
    .byte   YELLOW|$c
    .byte   YELLOW|$e
    .byte   YELLOW|$8
    .byte   YELLOW|$8
GrapesCol
    .byte   PURPLE|$a
    .byte   PURPLE|$c
    .byte   PURPLE|$c
    .byte   PURPLE|$c
    .byte   PURPLE|$a
    .byte   PURPLE|$c
    .byte   0
    .byte   0
    .byte   PURPLE|$c
    .byte   PURPLE|$c
    .byte   PURPLE|$c
    .byte   PURPLE|$c
    .byte   PURPLE|$e
    .byte   YELLOW|$6
PearCol
    .byte   GREEN_YELLOW|$6
    .byte   GREEN_YELLOW|$8
    .byte   GREEN_YELLOW|$a
    .byte   GREEN_YELLOW|$a
    .byte   GREEN_YELLOW|$a
    .byte   GREEN_YELLOW|$c
    .byte   0
    .byte   0
    .byte   GREEN_YELLOW|$c
    .byte   GREEN_YELLOW|$c
    .byte   GREEN_YELLOW|$c
    .byte   GREEN_YELLOW|$e
    .byte   BROWN|$6
    .byte   BROWN|$6

HumanColMask        ; Player sprite shading
    .byte   $f6
    .byte   $f8
    .byte   $f8
    .byte   $fa
    .byte   $fa
    .byte   $fa     ; immediate load in kernel!
    .byte   0       ; pellet area
    .byte   0       ; pellet area
    .byte   $fa
    .byte   $fa
    .byte   $fa
    .byte   $fc
    .byte   $fc
    .byte   $fe

GreyCol
    ds      GFX_H, AI_LUM + 2

AIColMask ; MUST be behind enemy colors!
    ds      GFX_H, $f0|AI_LUM       ; is it OK to cross a page?
    CHECKPAGE ColorTbls

    ALIGN_FREE_LBL 256, "EnemyGfx"

EnemyGfx
EnemyGfx0
    .byte   %01010101
    .byte   %00101010
    .byte   %01111111
    .byte   %01111111
    .byte   %01111111
    .byte   %01111111
    .byte   %01001001
    .byte   %01011011
    .byte   %01011011
    .byte   %01001001
    .byte   %01111111
    .byte   %00111110
    .byte   %00111110
    .byte   %00011100
EnemyGfx1
    .byte   %00101010
    .byte   %01010101
    .byte   %01111111
    .byte   %01111111
    .byte   %01111111
    .byte   %01111111
    .byte   %01001001
    .byte   %01011011
    .byte   %01011011
    .byte   %01001001
    .byte   %01111111
    .byte   %00111110
    .byte   %00111110
    .byte   %00011100
EnemyDarkGfx0
    .byte   %01010101
    .byte   %01010101
    .byte   %01111111
    .byte   %00101010
    .byte   %01010101
    .byte   %01111111
    .byte   %01111111
    .byte   %01101011
    .byte   %01101011
    .byte   %01111111
    .byte   %01111111
    .byte   %00111110
    .byte   %00111110
    .byte   %00011100
EnemyDarkGfx1
    .byte   %00101010
    .byte   %01010101
    .byte   %01111111
    .byte   %00101010
    .byte   %01010101
    .byte   %01111111
    .byte   %01111111
    .byte   %01101011
    .byte   %01101011
    .byte   %01111111
    .byte   %01111111
    .byte   %00111110
    .byte   %00111110
    .byte   %00011100
EnemyEyesGfx            ; can be overlapped
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00100100
    .byte   %00100100
    .byte   %00110110
    .byte   %00110110
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000

CherryGfx
    .byte   %00000110
    .byte   %00001111
    .byte   %01101011
    .byte   %11101111
    .byte   %10101111
    .byte   %11101111
    .byte   %11110110
    .byte   %11110100
    .byte   %01100000
    .byte   %00110010
    .byte   %00011010
    .byte   %00001101
    .byte   %00000111
    .byte   %00000011
OrangeGfx
    .byte   %00011100
    .byte   %00111110
    .byte   %00111110
    .byte   %01111011
    .byte   %01111101
    .byte   %01111101
    .byte   %01111101
    .byte   %01111111
    .byte   %01111111
    .byte   %01111111
    .byte   %00111110
    .byte   %00011100
    .byte   %00011000
    .byte   %00110100
StrawberryGfx
    .byte   %00001000
    .byte   %00011100
    .byte   %00011100
    .byte   %00110110
    .byte   %00111110
    .byte   %01111011
    .byte   %01111111
    .byte   %01011111
    .byte   %01110111
    .byte   %01111101
    .byte   %01101111
    .byte   %01111111
    .byte   %00110110
    .byte   %00011100
AppleGfx
    .byte   %00010100
    .byte   %00111110
    .byte   %00111110
    .byte   %01111011
    .byte   %01111101
    .byte   %01111101
    .byte   %01111101
    .byte   %01111111
    .byte   %01111111
    .byte   %01111111
    .byte   %01111111
    .byte   %00110110
    .byte   %00001000
    .byte   %00001000
MelonGfx
    .byte   %00111100
    .byte   %01111110
    .byte   %01111010
    .byte   %01111110
    .byte   %11111111
    .byte   %11110101
    .byte   %11111111
    .byte   %11111111
    .byte   %11111101
    .byte   %11110111
    .byte   %01111110
    .byte   %01111110
    .byte   %01111110
    .byte   %00111100
BananaGfx
    .byte   %00111000
    .byte   %01111100
    .byte   %01111110
    .byte   %00011110
    .byte   %00001111
    .byte   %00000111
    .byte   %00000111
    .byte   %00000011
    .byte   %00000011
    .byte   %00000011
    .byte   %00000011
    .byte   %00000010
    .byte   %00000010
    .byte   %00000100
GrapesGfx
    .byte   %00001000
    .byte   %00010100
    .byte   %00011100
    .byte   %00010100
    .byte   %00101010
    .byte   %00111110
    .byte   %00111010
    .byte   %00111110
    .byte   %01010101
    .byte   %01101011
    .byte   %01111111
    .byte   %01111111
    .byte   %00101010
    .byte   %00001100
PearGfx
    .byte   %00011000
    .byte   %00111100
    .byte   %01110110
    .byte   %01111010
    .byte   %01111010
    .byte   %01111110
    .byte   %01111110
    .byte   %00111100
    .byte   %00111100
    .byte   %00111100
    .byte   %00011000
    .byte   %00011000
    .byte   %00001000
    .byte   %00001100
    CHECKPAGE EnemyGfx

;    ALIGN_FREE_LBL 256, "PlayerPtr"

; TODO: all graphic pointers and colors have to be moved into theme files!
; player animation sequence:
PlayerPtr
    .byte   <PlayerGfx1, <PlayerGfx1, <PlayerGfx0, <PlayerGfx0
    .byte   <PlayerGfx1, <PlayerGfx1, <PlayerGfx2, <PlayerGfx2
    .byte   <PlayerGfx1 ; one extra for left moving
    CHECKPAGE PlayerPtr

BonusPtr
; (Cherry, Strawberry, Orange, Apple, Melon, Grapes, Banana, Pear)
    .byte   <PearGfx
    .byte   <CherryGfx, <StrawberryGfx, <OrangeGfx, <AppleGfx
    .byte   <MelonGfx, <GrapesGfx, <BananaGfx
    CHECKPAGE BonusPtr
BonusColPtr
    .byte   <PearCol
    .byte   <CherryCol, <StrawberryCol, <OrangeCol, <AppleCol
    .byte   <MelonCol, <GrapesCol, <BananaCol
    CHECKPAGE BonusColPtr

    ALIGN_FREE_LBL 256, "PlayerGfx"

PlayerGfx
PlayerGfx0
    .byte   %00011100
    .byte   %00111110
    .byte   %00111110
    .byte   %01111111
    .byte   %01111111
    .byte   %01111111
    .byte   %01111111
    .byte   %01111111
    .byte   %01111111
    .byte   %01101111
    .byte   %01101111
    .byte   %00111110
    .byte   %00111110
    .byte   %00011100
GFX_H = . - PlayerGfx0
PlayerGfx1
    .byte   %00011100
    .byte   %00111110
    .byte   %00111110
    .byte   %01111111
    .byte   %01111100
    .byte   %01111000
    .byte   %01110000
    .byte   %01110000
    .byte   %01111000
    .byte   %01101100
    .byte   %01101111
    .byte   %00111110
    .byte   %00111110
    .byte   %00011100
PlayerGfx2
    .byte   %00011100
    .byte   %00111110
    .byte   %00111100
    .byte   %01111100
    .byte   %01111000
    .byte   %01111000
    .byte   %01110000
    .byte   %01110000
    .byte   %01111000
    .byte   %01101000
    .byte   %01101100
    .byte   %00111100
    .byte   %00111110
    .byte   %00011100

PlayerDeadGfx0
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00011100
    .byte   %00111110
    .byte   %00111110
    .byte   %01111111
    .byte   %01110111
    .byte   %01110111
    .byte   %01100011
    .byte   %01100011
    .byte   %01000001
    .byte   %01000001
PlayerDeadGfx1
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00010100
    .byte   %00111110
    .byte   %01111111
    .byte   %01111111
    .byte   %01100011
    .byte   %01000001
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
PlayerDeadGfx2
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00110110
    .byte   %01111111
    .byte   %01111111
    .byte   %01111111
    .byte   %01111111
    .byte   %00011100
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
PlayerDeadGfx3
    .byte   %00000000
    .byte   %00110110
    .byte   %01111111
    .byte   %01111111
    .byte   %01111111
    .byte   %00111110
    .byte   %00011100
    .byte   %00011100
    .byte   %00001000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
PlayerDeadGfx4
    .byte   %00000000
    .byte   %00010100
    .byte   %00111110
    .byte   %00011100
    .byte   %00011100
    .byte   %00011100
    .byte   %00011100
    .byte   %00001000
    .byte   %00001000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
PlayerDeadGfx
    .byte   %00100010
    .byte   %00010100
    .byte   %01000000
    .byte   %00100001
    .byte   %00000010
    .byte   %01100000
    .byte   %00000111
    .byte   %01100000
    .byte   %00000111
    .byte   %00100000
    .byte   %01000010
    .byte   %00000001
    .byte   %00010100
    .byte   %00100010
    CHECKPAGE PlayerGfx
