; *** Data overlapping optimized by DOOD - v0.91 ***
; (C) 2013 Thomas Jentzsch
; Parameters: c:\Users\Thomas\Documents\Atari\dev\Dasm\Projects\PacLines\DOOD v0.91.exe font.inc -a -c -s

DigitGfx ; font is "Pac-Man"
Five ; @$0 = 0
  .byte %01111100
  .byte %11111110
  .byte %11000110
  .byte %00000110
  .byte %00000110
  .byte %11111110
  .byte %11111100
  .byte %11000000
  .byte %11111100
  .byte %11111100
FONT_H = . - Five

Letter_N ; @$a = 10
  .byte %00110100
  .byte %00110100
  .byte %01110000
  .byte %01110000
  .byte %11110000
  .byte %11110000
  .byte %10110000
  .byte %10110000
;  .byte %00110000
;  .byte %00110000
Seven ; @$12 = 18
  .byte %00110000
  .byte %00110000
  .byte %00110000
  .byte %00110000
  .byte %00111000
  .byte %00011100
  .byte %00001110
  .byte %11000110
;  .byte %11111110
;  .byte %11111110
Two ; @$1a = 26
  .byte %11111110
  .byte %11111110
  .byte %11100000
  .byte %01111000
  .byte %00111100
  .byte %00011110
  .byte %00001110
  .byte %11000110
  .byte %11111110
;  .byte %01111100
Eight ; @$23 = 35
  .byte %01111100
  .byte %11111110
  .byte %10000110
  .byte %10011110
  .byte %01111110
  .byte %11111000
  .byte %11100100
  .byte %11000100
  .byte %11111100
;  .byte %01111000
Nine ; @$2c = 44
  .byte %01111000
  .byte %01111100
  .byte %00001110
  .byte %00000110
  .byte %01111110
  .byte %11111110
  .byte %11000110
  .byte %11000110
  .byte %11111110
;  .byte %01111100
Six ; @$35 = 53
  .byte %01111100
  .byte %11111110
  .byte %11000110
  .byte %11000110
  .byte %11111110
  .byte %11111100
  .byte %11000000
  .byte %11100000
  .byte %01111100
  .byte %00111100

Letter_I ; @$4b = 75
  .byte %11110000
  .byte %11110000
  .byte %01100000
  .byte %01100000
  .byte %01100000
  .byte %01100000
  .byte %01100000
  .byte %01100000
  .byte %11110000
  .byte %11110000

Four ; @$55 = 85
  .byte %00001100
  .byte %00001100
  .byte %11111110
  .byte %11111110
  .byte %11001100
  .byte %11001100
  .byte %01101100
  .byte %00111100
  .byte %00111100
  .byte %00011100

BonusGfx ; @$5f = 95
  .byte %00000100
  .byte %01001010
  .byte %10101110
  .byte %11101110
  .byte %11100100
  .byte %01000000
  .byte %00000100
  .byte %00110110
  .byte %00011010
  .byte %00000110

Zero ; @$69 = 105
  .byte %00111000
  .byte %01111100
  .byte %11100100
  .byte %11000110
  .byte %11000110
  .byte %11000110
  .byte %11000110
  .byte %11001110
  .byte %01111100
  .byte %00111000

Letter_L ; @$73 = 115
  .byte %11111011
  .byte %11111011
  .byte %11000011
  .byte %11000011
  .byte %11000011
  .byte %11000011
  .byte %11000011
  .byte %11000011
  .byte %11000011
  .byte %11000011

Letter_H ; @$7d = 125
  .byte %01100110
  .byte %01100110
  .byte %01100110
  .byte %01100110
  .byte %01111110
  .byte %01111110
  .byte %01100110
  .byte %01100110
  .byte %01100110
  .byte %01100110

Three ; @$87 = 135
  .byte %01111100
  .byte %11111110
  .byte %11000110
  .byte %00000110
  .byte %00111110
  .byte %00111100
  .byte %00011000
  .byte %00001100
;  .byte %01111110
;  .byte %01111110
One ; @$8f = 143
  .byte %01111110
  .byte %01111110
  .byte %00011000
  .byte %00011000
  .byte %00011000
  .byte %00011000
  .byte %00011000
  .byte %00111000
  .byte %00111000
  .byte %00011000

PowerChar ; @$99 = 153
  .byte %00000000
  .byte %00000000
  .byte %00011000
  .byte %00011000
  .byte %00111100
  .byte %00111100
  .byte %00011000
  .byte %00011000
;  .byte %00000000
;  .byte %00000000
PelletChar ; @$a1 = 161
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00111100
  .byte %00111100
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00000000
                       ; @$ab = 171
DotChar ; @$3f = 63
  .byte %10000000
  .byte %10000000
;  .byte %00000000
;  .byte %00000000
;  .byte %00000000
;  .byte %00000000
;  .byte %00000000
;  .byte %00000000
;  .byte %00000000
;  .byte %00000000
Blank ; @$41 = 65
; 171/190 (90.0%)
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