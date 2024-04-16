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


;===============================================================================
; C O L O R - C O N S T A N T S
;===============================================================================

; general color constants
BLACK           = $00
WHITE           = $0e
  IF NTSC_COL
YELLOW          = $10
BROWN           = $20
ORANGE          = $30
RED             = $40
MAUVE           = $50
VIOLET          = $60
PURPLE          = $70
BLUE            = $80
BLUE_CYAN       = $90
CYAN            = $a0
CYAN_GREEN      = $b0
GREEN           = $c0
GREEN_YELLOW    = $d0
GREEN_BEIGE     = $e0
BEIGE           = $f0
  ELSE
YELLOW          = $20
BROWN           = YELLOW
ORANGE          = $40
RED             = $60
MAUVE           = $80
VIOLET          = $a0
PURPLE          = $c0
BLUE            = $d0
BLUE_CYAN       = $b0
CYAN            = $90
CYAN_GREEN      = $70
GREEN           = $50
GREEN_YELLOW    = $30
GREEN_BEIGE     = GREEN_YELLOW
BEIGE           = YELLOW
  ENDIF

    LIST ON
