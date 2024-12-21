DigitGfx ; font is "Level-Up"
Four
    .byte   %00001100
    .byte   %00001100
    .byte   %11111110
    .byte   %11111110
    .byte   %11001100
    .byte   %11101100
    .byte   %01101100
    .byte   %01111100
    .byte   %00111100
    .byte   %00111100
FONT_H = . - Four

Seven
    .byte   %01110000
    .byte   %01110000
    .byte   %01110000
    .byte   %01110000
    .byte   %01111000
    .byte   %00111100
    .byte   %00011110
    .byte   %00001110
;    .byte   %01111111
;    .byte   %01111111
Two
    .byte   %11111110
    .byte   %11111110
    .byte   %11110000
    .byte   %01111000
    .byte   %00111100
    .byte   %00011110
    .byte   %00001110
    .byte   %11001110
    .byte   %11111110
;    .byte   %00111110
Six
    .byte   %01111100
    .byte   %11111110
    .byte   %11100110
    .byte   %11100110
    .byte   %11111110
    .byte   %11111100
    .byte   %11100000
    .byte   %11100110
    .byte   %11111110
;    .byte   %00111110
Three
    .byte   %01111100
    .byte   %11111110
    .byte   %11001110
    .byte   %00001110
    .byte   %00111100
    .byte   %00111100
    .byte   %00001110
    .byte   %11001110
    .byte   %11111110
;    .byte   %00111110
Nine
    .byte   %01111100
    .byte   %11111110
    .byte   %11001110
    .byte   %00001110
    .byte   %01111110
    .byte   %11111110
    .byte   %11001110
    .byte   %11001110
    .byte   %11111110
;    .byte   %00111110
Eight
    .byte   %01111100
    .byte   %11111110
    .byte   %11001110
    .byte   %11001110
    .byte   %01111100
    .byte   %01111100
    .byte   %11001110
    .byte   %11001110
    .byte   %11111110
;    .byte   %00111110
Zero
    .byte   %01111100
    .byte   %11111110
    .byte   %11000110
    .byte   %11000110
    .byte   %11010110
    .byte   %11010110
    .byte   %11000110
    .byte   %11000110
    .byte   %11111110
;    .byte   %00111110
Five
    .byte   %01111100
    .byte   %11111110
    .byte   %11001110
    .byte   %00001110
    .byte   %01111110
    .byte   %11111100
    .byte   %11000000
    .byte   %11000000
    .byte   %11111110
    .byte   %11111110

One
    .byte   %00011000
    .byte   %00011000
    .byte   %00011000
    .byte   %00011000
    .byte   %00011000
    .byte   %00011000
    .byte   %00011000
    .byte   %01111000
    .byte   %01111000
    .byte   %00111000

Letter_L
    .byte   %11111011
    .byte   %11111011
    .byte   %11000011
    .byte   %11000011
    .byte   %11000011
    .byte   %11000011
    .byte   %11000011
    .byte   %11000000
    .byte   %11000000
    .byte   %11000000
Letter_I
    .byte   %11000000
    .byte   %11000000
    .byte   %11000000
    .byte   %11000000
    .byte   %11000000
    .byte   %11000000
    .byte   %11000000
    .byte   %00000000
    .byte   %11000000
    .byte   %11000000
Letter_H
    .byte   %01100110
    .byte   %01100110
    .byte   %01100110
    .byte   %01100110
    .byte   %01111110
    .byte   %01111110
    .byte   %01100110
    .byte   %01100110
    .byte   %01100110
    .byte   %01100110
BonusGfx
    .byte   %00000100
    .byte   %01001010
    .byte   %10101110
    .byte   %11101110
    .byte   %11100100
    .byte   %01000000
    .byte   %00000100
    .byte   %00110110
    .byte   %00011010
    .byte   %00000110
Letter_N
    .byte   %01101100
    .byte   %01101100
    .byte   %01100000
    .byte   %01100000
    .byte   %01100000
    .byte   %11100000
    .byte   %11000000
    .byte   %00000000
;    .byte   %00000000
;    .byte   %00000000
PowerChar
    .byte   %00000000
    .byte   %00000000
    .byte   %00011000
    .byte   %00011000
    .byte   %00111100
    .byte   %00111100
    .byte   %00011000
    .byte   %00011000
;    .byte   %00000000
;    .byte   %00000000
PelletChar
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00111100
    .byte   %00111100
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
    .byte   %00000000
DotChar
    .byte   %11000000
    .byte   %11000000
;    .byte   %00000000
;    .byte   %00000000
;    .byte   %00000000
;    .byte   %00000000
;    .byte   %00000000
;    .byte   %00000000
;    .byte   %00000000
;    .byte   %00000000
Blank
    ds      GFX_H - (GFX_H-POWER_H)/2, 0
EnaBlTbl               
    ds      (GFX_H-POWER_H)/2, 0
;    ds      1, %10
;    ds      4, 0
;    ds      1, %10
    ds      POWER_H, %10
    ds      (GFX_H-POWER_H)/2, 0
DIGIT_GFX_LEN = . - DigitGfx
    CHECKPAGE_DATA_LBL DigitGfx, "DigitGfx"