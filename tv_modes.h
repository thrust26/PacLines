    LIST OFF

;===============================================================================
; A S S E M B L E R - S W I T C H E S
;===============================================================================

; Derived switches, do NOT change these!
  IFCONST TV_MODE
   IF TV_MODE = 1
NTSC            = 1
PAL60           = 0
PAL50           = 0
   ENDIF
   IF TV_MODE = 2
NTSC            = 0
PAL60           = 1
PAL50           = 0
   ENDIF
   IF TV_MODE = 3   ; not supported!
NTSC            = 0
PAL60           = 0
PAL50           = 1
   ENDIF
  ENDIF

; Derived switches, do NOT change these!
  IF NTSC
NTSC_COL        = 1
NTSC_TIM        = 1
  ENDIF
  IF PAL60
NTSC_COL        = 0
NTSC_TIM        = 1
  ENDIF

  IF NTSC_TIM
SCANLINES       = 262
FPS             = 60    ; 1.193.182 / SCANLINES / 76 (NTSC; PAL-M: 1.191.870)
  ELSE
SCANLINES       = 312
FPS             = 50    ; 1.182.298 / SCANLINES / 76 (PAL; SECAM: 1.187.500)
  ENDIF


;===============================================================================
; C O L O R - C O N S T A N T S
;===============================================================================

; general color constants
BLACK           = $00
WHITE           = $0e
  IF NTSC_COL               ; GREY
YELLOW          = $10       ; GOLD                    0
BROWN           = $20       ; ORANGE
ORANGE          = $30       ; RED-ORANGE              4
RED             = $40       ; PINK                    7
MAUVE           = $50       ; PURPLE                  1
VIOLET          = $60       ; PURPLE_BLUE
PURPLE          = $70       ; BLUE                    5
BLUE            = $80       ; BLUE
BLUE_CYAN       = $90       ; LT. BLUE                2
CYAN            = $a0       ; TORQUOISE
CYAN_GREEN      = $b0       ; GREEN-BLUE              6
GREEN           = $c0       ; GREEN
GREEN_YELLOW    = $d0       ; YELLOW_GREEN            3
GREEN_BEIGE     = $e0       ; ORANGE_GREEN ($1.5)
BEIGE           = $f0       ; LT. ORANGE ($2.5)
  ELSE                      ;
YELLOW          = $20       ;   0
BROWN           = YELLOW+$20;
ORANGE          = $40+$20   ;
RED             = $60       ;   4
MAUVE           = $80       ;   1
VIOLET          = $a0       ;
PURPLE          = $c0       ;   5
BLUE            = $d0       ;   2
BLUE_CYAN       = $b0       ;
CYAN            = $90       ;   6
CYAN_GREEN      = $70       ;   3
GREEN           = $50       ;
GREEN_YELLOW    = $30       ;   7
GREEN_BEIGE     = GREEN_YELLOW
BEIGE           = YELLOW    ;   ???
  ENDIF

; 2, 4, 6, 8, a, c, d, b, 9, 7, 5, 3
; 0     4  1     5     2     6     3    alt
; 0     4  1     5  2     6  3     7    neu
    LIST ON
