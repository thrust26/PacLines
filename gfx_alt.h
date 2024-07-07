PlayerGfx0
    .byte   %00011100
    .byte   %00111110
    .byte   %00111110
    .byte   %01111111
    .byte   %01111111
    .byte   %01111111
    .byte   %00001000
    .byte   %00001000
    .byte   %01111111
    .byte   %01111111
    .byte   %01111111
    .byte   %00111110
    .byte   %00111110
    .byte   %00011100
GFX_H = . - PlayerGfx0
PlayerGfx1
    .byte   %00011100
    .byte   %00011110
    .byte   %00011110
    .byte   %01001111
    .byte   %01101111
    .byte   %01101111
    .byte   %01111111
    .byte   %01111111
    .byte   %01111011
    .byte   %01111011
    .byte   %01111001
    .byte   %00111100
    .byte   %00111100
    .byte   %00011100
PlayerGfx2
    .byte   %00010100
    .byte   %00110110
    .byte   %00110110
    .byte   %01110111
    .byte   %01110111
    .byte   %01110111
    .byte   %01111111
    .byte   %01111111
    .byte   %01110111
    .byte   %01110111
    .byte   %01110111
    .byte   %00110110
    .byte   %00110110
    .byte   %00010100
PlayerGfx3
    .byte   %00011100
    .byte   %00111100
    .byte   %00111100
    .byte   %01111001
    .byte   %01111011
    .byte   %01111011
    .byte   %01111111
    .byte   %01111111
    .byte   %01101111
    .byte   %01101111
    .byte   %01001111
    .byte   %00011110
    .byte   %00011110
    .byte   %00011100



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
    .byte   %01111111
    .byte   %01111111
    .byte   %00111110
    .byte   %00111110
    .byte   %00011100
GFX_H = . - PlayerGfx0
PlayerGfx1
    .byte   %00011000
    .byte   %00111010
    .byte   %00111010
    .byte   %01111101
    .byte   %01111101
    .byte   %01111101
    .byte   %01111101
    .byte   %01111101
    .byte   %01111101
    .byte   %01111101
    .byte   %01111101
    .byte   %00111010
    .byte   %00111010
    .byte   %00011000
PlayerGfx2
    .byte   %00010100
    .byte   %00110110
    .byte   %00110110
    .byte   %01110111
    .byte   %01110111
    .byte   %01110111
    .byte   %01110111
    .byte   %01110111
    .byte   %01110111
    .byte   %01110111
    .byte   %01110111
    .byte   %00110110
    .byte   %00110110
    .byte   %00010100
PlayerGfx3
    .byte   %00001100
    .byte   %00101110
    .byte   %00101110
    .byte   %01011111
    .byte   %01011111
    .byte   %01011111
    .byte   %01011111
    .byte   %01011111
    .byte   %01011111
    .byte   %01011111
    .byte   %01011111
    .byte   %00101110
    .byte   %00101100
    .byte   %00001100



PlayerGfx0
    .byte   %00011100
    .byte   %00111110
    .byte   %00111110
    .byte   %01000001
    .byte   %00000000
    .byte   %00011100
    .byte   %01111111
    .byte   %01101011
    .byte   %01101011
    .byte   %01101011
    .byte   %01111111
    .byte   %00111110
    .byte   %00111110
    .byte   %00011100
GFX_H = . - PlayerGfx0
PlayerGfx1
    .byte   %00011100
    .byte   %00111110
    .byte   %00111110
    .byte   %00001111
    .byte   %00000111
    .byte   %01100111
    .byte   %01111111
    .byte   %00101111
    .byte   %00101111
    .byte   %00101111
    .byte   %01111111
    .byte   %00111110
    .byte   %00111110
    .byte   %00011100
PlayerGfx2
    .byte   %00011100
    .byte   %00111110
    .byte   %00111110
    .byte   %00111110
    .byte   %00011100
    .byte   %00011100
    .byte   %01111111
    .byte   %01011101
    .byte   %01011101
    .byte   %01011101
    .byte   %01111111
    .byte   %00111110
    .byte   %00111110
    .byte   %00011100
PlayerGfx3
    .byte   %00011100
    .byte   %00111110
    .byte   %00111110
    .byte   %01111000
    .byte   %01110000
    .byte   %01110011
    .byte   %01111111
    .byte   %01111010
    .byte   %01111010
    .byte   %01111010
    .byte   %01111111
    .byte   %00111110
    .byte   %00111100
    .byte   %00011100

PlayerPtr
  IF THEME_ALT_1
    .byte   <PlayerGfx3, <PlayerGfx3, <PlayerGfx2, <PlayerGfx2
    .byte   <PlayerGfx1, <PlayerGfx1, <PlayerGfx0, <PlayerGfx0
    .byte   <PlayerGfx3
  ENDIF
  IF THEME_ALT_2 | THEME_ALT_3
    .byte   <PlayerGfx3, <PlayerGfx3, <PlayerGfx2, <PlayerGfx2
    .byte   <PlayerGfx1, <PlayerGfx1, <PlayerGfx0, <PlayerGfx0
    .byte   <PlayerGfx3
  ENDIF
    CHECKPAGE PlayerPtr
