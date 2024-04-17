; Pac-Lines x 8
;
; (C)2024 - Thomas Jentzsch

    processor 6502
  LIST OFF
    include vcs.h
  LIST ON

; TODOs:
; o difficulty ramp up
;   x globally
;   x individually
;   + mixed (levels individual, speed global)
;   o trigger
;     ? time
;     ? individual level
;     + global level
;   - speeds
;     - enemies and players seperately (or both same speed?)
;     ? by value or index (with table)
; - AI
;   - player (not playing, controlled by AI)
; + bonus items
;   ? when should they appear?
;     + either at start of new level (delayed?)
;     ? or when power-up gets eaten (disapear at next level)
;   ? when shoud bonuses disappear?
;     ? disappear at border
;     ? when power-up gets eaten
;     + when level is finished
;     ? never (when eaten or new bonus appears)
; ? larger power-up pellet
; o flicker
;   ? all 3 objects
;   + Enemy and Bonus?
;   ? Player and Enemies when Bonus arrives (player is never over a pellet)?
; o display
;   o score
;     o display top score
;     o display score of player which changed level last
;     - display score of player which set new maximum level
;     ? color gradients (would need another 2 bytes RAM for a pointer)
;     ? hex score stored, converted to BCD (up to 65,535 instead 9,999)
;   - level
;   ? displayed player number
; ? pellets, wafers, dots...?
; ? wider enemies?
; - align eating with pellets
; - dead player graphics

; DONEs:
; + AI
;   + enemies (following player)
;     + hunting: move to player (no wrapping)
;     + running: move to side away from player
;     + dead   : move to far away side
; + bonus items
;   + based on current individual level
; + running enemies graphics/colors


;===============================================================================
; A S S E M B L E R - S W I T C H E S
;===============================================================================

VERSION         = $0002
BASE_ADR        = $f000     ; 4K

  IFNCONST TV_MODE ; manually defined here
NTSC            = 1
PAL60           = 0
PAL50           = 0 ; not supported!
  ENDIF

    include tv_modes.h

ILLEGAL         = 1
DEBUG           = 1

SAVEKEY         = 0 ; (-~220) support high scores on SaveKey
PLUSROM         = 0 ; (-~50)
RAND16          = 0 ; (-3) 16-bit random values
COLOR_LINES     = 1 ; (-~20) use different colors for the lines between players
EXTRA_GAP       = 1 ; (-4) one extra line between rows
TOP_SCORE       = 1 ; (-41) display top score (else player who last changed level)


;===============================================================================
; C O L O R - C O N S T A N T S
;===============================================================================

LINE_COL        = BLUE|6        ; !COLOR_LINES


;===============================================================================
; G A M E - C O N S T A N T S
;===============================================================================

EOR_RND         = $bd           ; %10110100 ($9c, $b4, $bd, $ca, $eb, $fc)

SCW             = 160
PELLET_H        = 2
POWER_H         = 6
NUM_PLAYERS     = 8

POWER_TIM       = 120

NUM_BONUS       = 8
X_BONUS_OFF     = $ff
BONUS_SHIFT     = 2                 ; 2 = every 4th level
BONUS_MASK      = (1<<BONUS_SHIFT)-1

INIT_PL_SPEED   = 80
DIFF_PL_SPEED   = 4
MAX_PL_SPEED    = 180               ;
INIT_EN_SPEED   = INIT_PL_SPEED-8
DIFF_EN_SPEED   = DIFF_PL_SPEED+2   ; -> equal speed after 4 levels
MAX_EN_SPEED    = 240               ; 33% faster than player

PELLET_PTS      = 1
POWER_PTS       = 5
ENEMY_PTS       = 10

NUM_TMPS        = 6
STACK_SIZE      = 6                 ; used in score & kernel row setup


;===============================================================================
; Z P - V A R I A B L E S
;===============================================================================

    SEG.U   variables
    ORG     $80

frameCnt        .byte       ; even: enemy drawn, odd: bonus drawn
randomLo        .byte
  IF RAND16 ;{
randomHi        .byte
  ENDIF ;}
levelLst        ds  NUM_PLAYERS
playerSpeed     .byte
playerSpeedSum  .byte
enemySpeed      .byte
enemySpeedSum   .byte
bonusSpeed      .byte       ; TODO: eliminate if bonus speed is always 50% of enemy speed
bonusSpeedSum   .byte
;---------------------------------------
playerDirs      .byte       ; 1 = left, 0 = right
enemyDirs       .byte       ; 1 = left, 0 = right
bonusDirs       .byte       ; 1 = left, 0 = right
;playerStates    .byte       ; 0 = alive, 1 = dead/score rolled  TODO
;playerHumans    .byte       ; 0 = human, 1 = AI TODO
enemyStates     .byte       ; 0 = alive, 1 = eyes
;---------------------------------------
xPlayerLst      ds  NUM_PLAYERS             ; 32 bytes
xEnemyLst       ds  NUM_PLAYERS
xBonusLst       ds  NUM_PLAYERS
xPowerLst       ds  NUM_PLAYERS
;---------------------------------------
pfLst           ds  NUM_PLAYERS*3           ; 24 bytes
pf01LeftLst     = pfLst
pf20MiddleLst   = pfLst+NUM_PLAYERS
pf12RightLst    = pfLst+NUM_PLAYERS*2
;---------------------------------------
powerTimLst     ds NUM_PLAYERS              ;  8 bytes
;---------------------------------------
scoreLst        ds  NUM_PLAYERS*2           ; 16 bytes
scoreLoLst      = scoreLst
scoreHiLst      = scoreLst+NUM_PLAYERS
  IF !TOP_SCORE
scorePlayerIdx  .byte                       ; index of displayed score's player
  ENDIF
;---------------------------------------
cxPelletBits    .byte                       ; temporary
cxSpriteBits    .byte                       ; temporary

RAM_END         = .

; temp vars and stack form a consecutive area:
tmpVars         = $100 - STACK_SIZE - NUM_TMPS


;===============================================================================
; M A C R O S
;===============================================================================

DEBUG_BYTES SET 0

  MAC DEBUG_BRK
    IF DEBUG
DEBUG_BYTES SET DEBUG_BYTES + 1
      brk                         ;
    ENDIF
  ENDM

  MAC NOP_IMM   ; skip 1 byte, 2 cycles
    .byte   $82
  ENDM

  MAC NOP_B     ; skip 1 byte, 3 cycles
    .byte   $04
  ENDM

  MAC NOP_W     ; skip 2 bytes, 4 cycles
    .byte   $0c
  ENDM

;  MAC BIT_B     ; skip 1 byte, 3 cycles
;    .byte   $24
;  ENDM
;
;  MAC BIT_W     ; skip 2 bytes, 4 cycles
;    .byte   $2c
;  ENDM

  MAC SLEEP
    IF {1} = 1
      ECHO "ERROR: SLEEP 1 not allowed !"
      END
    ENDIF
    IF {1} & 1
      nop $00
      REPEAT ({1}-3)/2
        nop
      REPEND
    ELSE
      REPEAT ({1})/2
        nop
      REPEND
    ENDIF
  ENDM

  MAC _CHECKPAGE ; internal, do not use directly
    IF >{1} != >{2}
      ECHO ""
     IF {3} != ""
      ECHO "ERROR: different pages! (", {3}, "=", {2}, ",", {1}, ")"
     ELSE
      ECHO "ERROR: different pages! (", {2}, ",", {1}, ")"
     ENDIF
      ECHO ""
      ERR
    ENDIF
  ENDM

  MAC CHECKPAGE_LBL
    _CHECKPAGE ., {1}, {2}
  ENDM

  MAC CHECKPAGE
    CHECKPAGE_LBL {1}, ""
  ENDM

  MAC CHECKPAGE_DATA_LBL
_ADDR SET . - 1 ; hack to convince DASM
    _CHECKPAGE _ADDR, {1}, {2}
  ENDM

  MAC CHECKPAGE_DATA
    CHECKPAGE_DATA_LBL {1}, ""
  ENDM

  MAC VERSION_STR
    .byte   ((VERSION & $f00) >> 8) + 48
    .byte   "."
    .byte   ((VERSION & $0f0) >> 4) + 48
    .byte   ((VERSION & $00f) >> 0) + 48
  ENDM

;---------------------------------------------------------------
; Free space macros
;---------------------------------------------------------------
ECHO_FREE SET 1     ; 1 = echo free space enabled
FREE_TOTAL SET 0    ; use only once

  MAC OUT_FREE
FREE_GAP$ SET - .
    {1} {2}
FREE_GAP$  SET FREE_GAP$  + .
FREE_TOTAL SET FREE_TOTAL + FREE_GAP$
   IF ECHO_FREE && FREE_GAP$ > 0
    ECHO "@", ., ": Gap:", [FREE_GAP$]d, "; Total:", [FREE_TOTAL]d, ";", {3}, {2}, {4}
   ENDIF
  ENDM

  MAC ALIGN_FREE_LBL
    LIST OFF
    OUT_FREE ALIGN, {1}, "ALIGN", {2}
    LIST ON
  ENDM

  MAC ALIGN_FREE
    LIST OFF
    ALIGN_FREE_LBL {1}, ""
  ENDM

  MAC COND_ALIGN_FREE_LBL ; space required, alignement, "label"
;    LIST OFF
   IF (>(. + {1} - 1)) > (>.)
    ALIGN_FREE_LBL {2}, {3}
   ENDIF
    LIST ON
  ENDM

  MAC COND_ALIGN_FREE ; space required, alignement
;    LIST OFF
    COND_ALIGN_FREE_LBL {1}, {2}, ""
  ENDM

  MAC ORG_FREE_LBL
    LIST OFF
    OUT_FREE ORG, {1}, "ORG", {2}
    LIST ON
  ENDM

  MAC ORG_FREE
    LIST OFF
    ORG_FREE_LBL {1}, ""
  ENDM

  MAC HEX2BCD ; hex value
; Hex2Bcd (good 0-99), 22 bytes, 26 cycles:
    tax                     ; 2         0..$63
    lsr                     ; 2
    lsr                     ; 2
    lsr                     ; 2
    lsr                     ; 2
    tay                     ; 2         0..3
    txa                     ; 2
    sed                     ; 2
    clc                     ; 2
    adc     #0              ; 2
    adc     BcdTbl,y        ; 4
    cld                     ; 2 = 26
  ENDM


;===============================================================================
; R O M - C O D E
;===============================================================================
    SEG     Bank0
    ORG     BASE_ADR

    ds  60, 0

;---------------------------------------------------------------
DrawScreen SUBROUTINE
;---------------------------------------------------------------
.scorePtrLst    = tmpVars
.scorePtr0      = .scorePtrLst      ; lowest digit
.scorePtr1      = .scorePtrLst+2
.scorePtr2      = .scorePtrLst+4
.scorePtr3      = .scorePtrLst+6
.scorePtr4      = .scorePtrLst+8
.scorePtr5      = .scorePtrLst+10   ; highest digit
.scoreCol       = cxPelletBits

.waitTim
    lda     INTIM
    bne     .waitTim
    sta     WSYNC
;---------------------------------------
    sta     HMOVE               ; 3
    sta     VBLANK              ; 3
    ldy     #FONT_H-1           ; 2 =  8
.loopScore                      ;           @60
    lda     .scoreCol           ; 3
    and     ScoreLums,y         ; 4
    sta     COLUP0              ; 3
    sta     COLUP1              ; 3
    sta     WSYNC               ; 3 = 16    zero cycles free!
;---------------------------------------
    lda     (.scorePtr5),y      ; 5
    sta     GRP0                ; 3
    lda     (.scorePtr4),y      ; 5
    sta     GRP1                ; 3
    lda     (.scorePtr3),y      ; 5
    sta     GRP0                ; 3 = 24
    lax     (.scorePtr0),y      ; 5
    txs                         ; 2
    lax     (.scorePtr2),y      ; 5
    lda     (.scorePtr1),y      ; 5
    stx     GRP1                ; 3 = 20    @44
    sta     GRP0                ; 3         @47
    tsx                         ; 2
    stx     GRP1                ; 3         @52
    sta     GRP0                ; 3 = 11
    dey                         ; 2
    bpl     .loopScore          ; 3/2= 5/4
    iny
    sty     GRP1
    sty     GRP0
    sty     GRP1
    sty     NUSIZ0
    sty     NUSIZ1
    sty     VDELP1
    sty     GRP1
    ldx     #$ff                ; 2
    txs                         ; 2
;---------------------------------------------------------------
.loopCnt    = tmpVars
.bonusFlag  = tmpVars+1         ; could be on stack,  if stack becomes larger
.ptrCol1    = tmpVars+2         ; same
.color0     = tmpVars+4         ; same
.ptr0       = $fa
.ptr1       = $fc
.ptrBl      = $fe

    lda     frameCnt            ; 3
    lsr                         ; 2
    lda     #$ff
    adc     #0
    sta     .bonusFlag          ;           0 = bonus frame, $ff = enemy frame

    lda     #>ColorTbls
    sta     .ptrCol1+1

    ldx     #NUM_PLAYERS-1
.loopKernels                    ;           @08
    stx     .loopCnt            ; 3
    sta     WSYNC
;---------------------------------------
    sty     GRP1                ; 3         Y == 0!
; draw lines between players:
  IF COLOR_LINES
    SLEEP   17-2
  ELSE
    SLEEP   17
  ENDIF

X_OFS   = 48
; prepare player:               ; 3         @20!
    lda     xPlayerLst,x        ; 4
    clc                         ; 2
    adc     #X_OFS              ; 2
    pha                         ; 3         x-pos
    lda     #0                  ; 2
    pha                         ; 3         index
;    lda     #0                  ; 2
    pha                         ; 3 = 19    color
; prepare enemy/bonus:
    lda     xBonusLst,x         ; 4
    ora     .bonusFlag          ; 3         0|$ff
    cmp     #X_BONUS_OFF        ; 2         if xBonus = OFF, draw enemy
BranchBonus
    bne     .positionBonus      ; 2/3
; position enemy:
    lda     xEnemyLst,x         ; 4
    adc     #X_OFS-1            ; 2
.contBonus                      ;   = 17    @56!
    pha                         ; 3         x-pos
    lda     #1                  ; 2
    pha                         ; 3         index
  IF COLOR_LINES
    lda     LineCols+1,x        ; 4
  ELSE
    lda     #LINE_COL           ; 2
  ENDIF
    pha                         ; 3 = 13    color
; prepare power-up (not drawn from stack):
    lda     xPowerLst,x         ; 4         x-pos
    ldx     #4                  ; 2 =  6    index
;---------------------------------------
    ldy     #0                  ; 2         color   @01
;    clc                         ; 2
    adc     #X_OFS              ; 2
    sec                         ; 2
EnterBranch
    bcs     .enterLoop          ; 3 =  9!   @08!

.positionBonus                  ;12
    adc     #X_OFS              ; 2
    bcc     .contBonus          ; 3 = 17
    CHECKPAGE BranchBonus

;---------------------------------------------------------------
.rightPos                       ;           @14
    sbc     #X_OFS+15*5         ; 2
.waitRight
    sbc     #$0f                ; 2
    bcs     .waitRight          ; 3/2       @34
    tay                         ; 2
    lda     HMoveTbl-$f1,y      ; 5!
    sta     HMP0,x              ; 4
    pla                         ; 4         color
    tay                         ; 2
    pla                         ; 4         index
    cpx     #1                  ; 2
    sta.wx  RESP0,x             ; 5 = 34..  @48..73!
    sta     WSYNC               ; 3
;---------------------------------------
    bcc     .exit               ; 2/4!
    tax                         ; 2
.next
    pla                         ; 4 =  8    @08     x-pos 48..208
.enterLoop                      ;
    sty     COLUBK              ; 3
    bmi     .rightPos           ; 2/3= 5/6  @13/14
    sbc     #X_OFS              ; 2                 CF == 1!
.waitLeft
    sbc     #$0f                ; 2
    bcs     .waitLeft           ; 3/2
    CHECKPAGE EnterBranch
    sta     RESP0,x             ; 4 =  10   @23..48!
    tay                         ; 2
    lda     HMoveTbl-$f1,y      ; 5!
    sta     HMP0,x              ; 4
    pla                         ; 4         color
    tay                         ; 2
    pla                         ; 4         index
    cpx     #1                  ; 2
    tax                         ; 2
    sta     WSYNC               ; 3 = 28
;---------------------------------------
  IF >. = >.next
    ECHO    ""
    ECHO    "ERROR: Must be in different pages!"
    ERR
  ENDIF
    bcs     .next               ; 4/2= 4/2  we need 4 cycles here!
.exit                           ;           @02/04
    ldx     #$ff                ; 2
    txs                         ; 2
  IF EXTRA_GAP
    sta     WSYNC
  ENDIF
  IF COLOR_LINES
    ldx     .loopCnt            ; 3
    lda     LineCols,x           ; 4
  ELSE
    lda     #LINE_COL           ; 2
  ENDIF
    sta     COLUBK              ; 3 = 11/13 (+2)
;--------------------------------------------------------------
TIM_1A
; setup sprite pointers:                    @11/13
    lda     frameCnt            ; 3         TODO: animate in sync with speed?
    and     #7                  ; 2
    tay                         ; 2
    lda     PlayerPtr,y         ; 4
    sta     .ptr0               ; 3 = 14
; setup player sprite direction:
  IF !COLOR_LINES
    ldx     .loopCnt            ; 3
  ENDIF
    ldy     #8-1                ; 2
    lda     playerDirs          ; 3
    and     Pot2Bit,x           ; 4
    beq     .right0             ; 2/3
    iny                         ; 2
.right0
    sty     REFP0               ; 3 = 15/16
    lda     PlayerCol,x         ; 4
    sta     .color0             ; 3 =  7
; setup power-up pointer:
    ldy     #<EnaBlTbl          ; 2
    lda     xPowerLst,x         ; 4
    bne     .showPower          ; 2/3
    ldy     #<DisBlTbl          ; 2
.showPower
    sty     .ptrBl              ; 3
TIM_1B
    sta     WSYNC               ; 3 = 15/16 @62..66
;---------------------------------------
    sta     HMOVE               ; 3
    lda     #0                  ; 2
;    lda     LineCols,x
    sta     COLUBK              ; 3 =  8

    bit     .bonusFlag          ; 3
    bmi     .drawEnemy          ; 2/3
    lda     xBonusLst,x         ; 4
    cmp     #X_BONUS_OFF        ; 2
    beq     .drawEnemy          ; 2/3=13/14
; draw bonus;
    lda     levelLst,x          ; 4
  REPEAT BONUS_SHIFT
    lsr                         ; 2/4/6     every 2nd/4th/8th level
  REPEND                        ;   =  6..10
    and     #NUM_BONUS-1        ; 2
    tay                         ; 2
    lda     BonusPtr,y          ; 4
    sta     .ptr1               ; 3
    lda     BonusColPtr,y       ; 4
    sta     .ptrCol1            ; 3
    ldy     #0                  ; 2
    sta     WSYNC               ; 3 = 23
;---------------------------------------
    beq     .setRefP1           ; 3 =  3

.drawEnemy                      ;14 = 14    @22
    lda     enemyStates         ; 3
    and     Pot2Bit,x           ; 4
    beq     .enemyAlive         ; 2/3
    ldy     #<EnemyEyesCol      ; 2
    sty     .ptrCol1            ; 3
    ldy     #<EnemyEyesGfx      ; 2
    bne     .setPtr1            ; 3

.enemyAlive                     ;10 = 10    @32
; setup colors:
    lda     powerTimLst,x       ; 4
    beq     .powerOff           ; 2/3
    ldy     #<EnemyColDark      ; 2
    cmp     #$50                ; 2
    bcs     .setColPtr1x        ; 2/3
    and     #$08                ; 2
    bne     .setColPtr1x        ; 2/3
    ldy     #<EnemyColBlink     ; 2
.setColPtr1x
    sty     .ptrCol1            ; 3 = 10..21
    ldy     #<EnemyDarkGfx0     ; 2
    lda     frameCnt            ; 3         TODO: animate in sync with speed?
    and     #$04                ; 2
    bne     .setPtr1            ; 2/3
    ldy     #<EnemyDarkGfx1     ; 2
    bne     .setPtr1            ; 3 = 14

.powerOff                       ; 7         @39
    ldy     EnemyColPtr,x       ; 4
    sty     .ptrCol1            ; 3
    ldy     #<EnemyGfx0         ; 2
    lda     frameCnt            ; 3         TODO: animate in sync with speed?
    and     #$04                ; 2
    bne     .setPtr1            ; 2/3
    ldy     #<EnemyGfx1         ; 2
.setPtr1                        ;   = 24/25 @63/64
    sty     .ptr1               ; 3
; 6 cycles free
    sta     WSYNC               ; 3 =  6
;---------------------------------------
; setup enemy reflection:
    ldy     #8-1                ; 2
    lda     enemyDirs           ; 3
    and     Pot2Bit,x           ; 4
    beq     .setRefP1           ; 2/3
    iny                         ; 2         moving left
.setRefP1
    sty     REFP1               ; 3 = 15/16
; setup some high-pointer:
    lda     #>SpriteGfx         ; 2
    sta     .ptr0+1             ; 3
    sta     .ptr1+1             ; 3
    sta     .ptrBl+1            ; 3
    sta     CXCLR               ; 3
    ldy     #GFX_H-1            ; 2 = 16
.loopTop                        ;           @32/28
    lda     (.ptr0),y           ; 5
    sta     GRP0                ; 3         VDELed!
    lda     (.ptrBl),y          ; 5
    sta     ENABL               ; 3         VDELed!
    lda     .color0             ; 3
    sbc     ColDiff0,y          ; 4
    tax                         ; 2
    lda     (.ptrCol1),y        ; 5
    sta     WSYNC               ; 3 = 33
;---------------------------------------
    stx     COLUP0              ; 3
    sta     COLUP1              ; 3
    lda     (.ptr1),y           ; 5
    sta     GRP1                ; 3
    lda     PfColTbl,y          ; 4
    sta     COLUPF              ; 3 = 21
    dey                         ; 2
    cpy     #(GFX_H+PELLET_H)/2 ; 2
    bcs     .loopTop            ; 3/2= 7/6
    SLEEP   28                  ;28
    ldx     .loopCnt            ; 3 = 31    @58!
; 1st pellet line:
    lda     (.ptr0),y           ; 5
    sta     GRP0                ; 3         VDELed!
    lda     (.ptr1),y           ; 5
    dey                         ; 2
    sta     GRP1                ; 3 = 18    @00!
;---------------------------------------
    lda     PfColTbl+1,y        ; 4
    sta     COLUPF              ; 3 = 10    @07
    lda     pf01LeftLst,x       ; 4
    and     #$aa                ; 2
    sta     PF0                 ; 3 =  9    @16
    eor     pf01LeftLst,x       ; 4
    sta     PF1                 ; 3 =  7    @23
    lda     pf20MiddleLst,x     ; 4
    and     #$aa                ; 2
    sta     PF2                 ; 3 =  9    @32
    eor     pf20MiddleLst,x     ; 4
    asl                         ; 2
    sta     PF0                 ; 3 =  9    @41
    lda     pf12RightLst,x      ; 4
    and     #$55                ; 2
    sta     PF1                 ; 3
    eor     pf12RightLst,x      ; 4
    sta     PF2                 ; 3 = 16    @57
; 2nd pellet line:
    lda     (.ptr0),y           ; 5
    sta     GRP0                ; 3         VDELed!
    lda     (.ptr1),y           ; 5
    dey                         ; 2
;---------------------------------------
    sta.w   GRP1                ; 3 = 18    @00
    lda     PfColTbl+1,y        ; 4
    sta     COLUPF              ; 3 = 23    @11
    lda     pf01LeftLst,x       ; 4
    and     #$aa                ; 2
    sta     PF0                 ; 3 =  9    @16
    eor     pf01LeftLst,x       ; 4
    sta     PF1                 ; 3 =  7    @23
    lda     pf20MiddleLst,x     ; 4
    and     #$aa                ; 2
    sta     PF2                 ; 3 =  9    @32
    eor     pf20MiddleLst,x     ; 4
    asl                       ; 2
    sta     PF0                 ; 3 =  9    @41
    lda     pf12RightLst,x      ; 4
    and     #$55                ; 2
    sta     PF1                 ; 3
    eor     pf12RightLst,x      ; 4
    sta     PF2                 ; 3 = 16    @57
; intermediate line:            ;
    lda     (.ptr0),y           ; 5
    sta     GRP0                ; 3         VDELed!
    lax     (.ptr1),y           ; 5
    lda     .color0             ; 3
    sbc     #$4                 ; 2 = 18    @75         ColDiff0,y (Y = 5)
;---------------------------------------
    sta     COLUP0              ; 3 =  3    @02
    lda     (.ptrCol1),y        ; 5
    sta     COLUP1              ; 3         @10
    stx     GRP1                ; 3 = 11    @13
    lda     #$02                ; 2
    sta     COLUPF              ; 3 =  5    @18
    sta     PF0                 ; 3         @21
    lda     #0                  ; 2
    sta     PF1                 ; 3         @26
    sta     PF2                 ; 3 = 11    @29
    dey
.loopBtm                        ;
    lda     (.ptr0),y           ; 5
    sta     GRP0                ; 3         VDELed!
    lda     (.ptrBl),y          ; 5
    sta     ENABL               ; 3         VDELed!
    lda     .color0             ; 3
    sbc     ColDiff0,y          ; 4
    tax                         ; 2
    lda     (.ptrCol1),y        ; 5
    sta     WSYNC               ; 3 = 35
;---------------------------------------
    stx     COLUP0              ; 3
    sta     COLUP1              ; 3
    lda     (.ptr1),y           ; 5
    sta     GRP1                ; 3 = 14
    lda     #$08                ; 2
    sta     COLUPF              ; 3 =  5    @19
    dey
    bpl     .loopBtm
    iny
    sty     GRP0                ;           VDELed!
    lda     CXP0FB
    asl
    rol     cxPelletBits
    ldx     .loopCnt
    lda     CXPPMM              ;
    asl
    rol     cxSpriteBits
    dex
    bmi     .exitLoop           ; 2/3
    jmp     .loopKernels        ; 3

.exitLoop
    sta     WSYNC
;---------------------------------------
    sty     GRP1                ;           Y = 0
; draw bottom lines:
  IF COLOR_LINES
    lda     #BLACK|$8
  ELSE
    lda     #LINE_COL
  ENDIF
    sta     WSYNC
;---------------------------------------
    sta     WSYNC
;---------------------------------------
    sta     COLUBK
    sta     WSYNC
;---------------------------------------
    sty     COLUBK
  IF EXTRA_GAP
    sta     WSYNC
  ENDIF
  IF COLOR_LINES
    lda     #WHITE
  ENDIF
    sta     WSYNC
;---------------------------------------
    sta     COLUBK
    ldx     #2
    sta     WSYNC
;---------------------------------------
    sty     COLUBK
    stx     VBLANK
    jmp     ContDrawScreen
; /DrawScreen

;---------------------------------------------------------------
Start SUBROUTINE
;---------------------------------------------------------------
    lda     #0
    tax
    cld                     ; clear BCD math bit
.clearLoop
    dex
    txs
    pha
    bne     .clearLoop

    lda     INTIM           ; randomize
    ora     #$01
    sta     randomLo

    jsr     GameInit

MainLoop
;---------------------------------------------------------------
VerticalBlank SUBROUTINE
;---------------------------------------------------------------
    lda     #%1110          ; each '1' bits generate a VSYNC ON line (bits 1..3)
.loopVSync
    sta     WSYNC           ; 1st '0' bit resets Vsync, 2nd '0' bit exits loop
    sta     VSYNC
    lsr
    bne     .loopVSync      ; branch until VSYNC has been reset

    inc     frameCnt

  IF NTSC_TIM
    lda     #44-1
  ELSE
    lda     #77
  ENDIF
    sta     TIM64T

.tmpSpeed       = tmpVars
.playerSpeed    = tmpVars+1
.enemySpeed     = tmpVars+2
.bonusSpeed     = tmpVars+3

    lda     #0
    sta     .playerSpeed
    lda     playerSpeed
    asl
    rol     .playerSpeed
    adc     playerSpeedSum
    sta     playerSpeedSum
    bcc     .skipIncPlayer
    inc     .playerSpeed
.skipIncPlayer
    lda     #0
    sta     .enemySpeed
    lda     enemySpeed
    asl
    rol     .enemySpeed
    adc     enemySpeedSum
    sta     enemySpeedSum
    bcc     .skipIncEnemy
    inc     .enemySpeed
.skipIncEnemy

    lda     frameCnt
    lsr
    bcc     .skipBonusSpeed
    lda     #0
    sta     .bonusSpeed
    lda     bonusSpeed
    asl
    rol     .bonusSpeed
    adc     bonusSpeedSum
    sta     bonusSpeedSum
    bcc     .skipIncBonus
    inc     .bonusSpeed
.skipIncBonus
.skipBonusSpeed

    ldx     #NUM_PLAYERS-1
.loopMove
    lda     playerDirs
    and     Pot2Bit,x
    cmp     #1
    lda     xPlayerLst,x
    bcs     .negSpeedPlayer
;    clc
    adc     .playerSpeed
    cmp     #SCW
    bcc     .setXPlayer
    lda     #0
    beq     .setXPlayer

.negSpeedPlayer
;    sec
    sbc     .playerSpeed
    bcs     .setXPlayer
    lda     #SCW-1
.setXPlayer
    sta     xPlayerLst,x
; *** Enemy AI ***
    lda     .enemySpeed
    sta     .tmpSpeed
    lda     enemyStates
    and     Pot2Bit,x
    beq     .alive
    asl     .tmpSpeed       ; eyes move at double enemy speed
; dead, eyes:
    and     enemyDirs       ; continue moving into old direction
    beq     .posSpeedEyes
    bne     .negSpeedEyes

.alive
    lda     xEnemyLst,x
    cmp     xPlayerLst,x
    lda     enemyDirs
    ldy     powerTimLst,x
    bcs     .negSpeedEnemy
    bne     .negSpeedEnemyTim
.posSpeedEnemyTim
    and     Pot2Mask,x
    sta     enemyDirs
.posSpeedEyes
    clc
    lda     xEnemyLst,x
    adc     .tmpSpeed
    cmp     #SCW-9
    bcc     .setXEnemy
    lda     #SCW-8
    bcs     .reviveEnemy

.negSpeedEnemy
    bne     .posSpeedEnemyTim
.negSpeedEnemyTim
    ora     Pot2Bit,x
    sta     enemyDirs
.negSpeedEyes
    sec
    lda     xEnemyLst,x
    sbc     .tmpSpeed
    bcs     .setXEnemy
    lda     #0
.reviveEnemy
    tay
    lda     enemyStates
    and     Pot2Mask,x
    sta     enemyStates
    tya
.setXEnemy
    sta     xEnemyLst,x

    lda     frameCnt
    lsr
    bcc     .skipBonus
    lda     xBonusLst,x
    cmp     #X_BONUS_OFF        ; disabled bonus?
    beq     .skipBonus          ;  yes, do not move
    tay
    lda     bonusDirs
    and     Pot2Bit,x
    cmp     #1
    tya
    bcs     .negSpeedBonus
;    clc
    adc     .bonusSpeed
    cmp     #SCW
    bcc     .setXBonus
    lda     #0
    beq     .setXBonus

.negSpeedBonus
;    sec
    sbc     .bonusSpeed
    bcs     .setXBonus
    lda     #SCW-1
.setXBonus
    sta     xBonusLst,x
.skipBonus
; update power-up timer:
    lda     powerTimLst,x
    beq     .skipTim
    sec
    sbc     .tmpSpeed           ; decrease in sync with speed
    bcs     .timOk
    lda     #0
.timOk
    sta     powerTimLst,x
.skipTim
    jsr     NextRandom
    eor     frameCnt
    cmp     #$fe
    bcc     .skipReverseDir
    lda     playerDirs
    eor     Pot2Bit,x
    sta     playerDirs
.skipReverseDir
    dex
    bmi     .exitLoop
    jmp     .loopMove

.exitLoop

; ******************** Setup Score ********************
.scoreLst   = tmpVars
  IF TOP_SCORE
.maxLo          = .scoreLst
.maxHi          = .scoreLst+1
.player         = .scoreLst+2
  ENDIF
.scoreLo        = .scoreLst
.scoreMid       = .scoreLst+1
.scoreHi        = .scoreLst+2
.scorePtrLst    = tmpVars           ; overlapping with .scoreLst!
.scorePtr0      = .scorePtrLst      ; lowest digit
.scorePtr1      = .scorePtrLst+2
.scorePtr2      = .scorePtrLst+4
.scorePtr3      = .scorePtrLst+6
.scorePtr4      = .scorePtrLst+8
.scorePtr5      = .scorePtrLst+10   ; highest digit
.scoreCol       = cxPelletBits

; determine player with maximum score:
DEBUG0
  IF TOP_SCORE
    lda     #0
    sta     .player
    sta     .maxLo
    sta     .maxHi
    ldx     #NUM_PLAYERS-1
.loopMax
    ldy     scoreLoLst,x
    lda     scoreHiLst,x
    cmp     .maxHi
    bcc     .nextMax
    bne     .setMax
    cpy     .maxLo
    bcc     .nextMax
.setMax
    sta     .maxHi
    sty     .maxLo
    stx     .player
.nextMax
    dex
    bpl     .loopMax
; display maximum score:
    ldx     .player
  ELSE
    ldx     scorePlayerIdx
    lda     scoreLoLst,x
    sta     .scoreLo
    lda     scoreHiLst,x
    sta     .scoreMid
  ENDIF
    lda     frameCnt
    cmp     #$c0            ; 3/4 score, 1/4 level
    bcc     .displayScore
;    lda     #ID_BLANK|(ID_BLANK<<4)
    lda     #0
    sta     .scoreMid
    lda     levelLst,x
;    clc
    adc     #1-1
    HEX2BCD
    sta     .scoreLo
  IF TOP_SCORE
    ldx     .player
  ELSE
    ldx     scorePlayerIdx
  ENDIF
.displayScore
    lda     PlayerCol,x
    sta     .scoreCol
    inx                     ; display player number
    txa
    asl
    asl
    asl
    asl
    ora     #ID_BLANK
    sta     .scoreHi
; setup score into temporary pointers:
    ldx     #(6-1)*2
.loopScores
    cpx     #(4-1)*2        ; start of points?
    bne     .skipV          ;  no, skip
    bit     Vectors+1       ; set V-flag to remove leading zeroes, bit 6 must be set!
.skipV
    txa
    lsr
    lsr
    tay
    lda     .scoreLst,y
    pha
    lsr
    lsr
    lsr
    lsr
    tay
    bne     .skipHiZero
    bvc     .skipHiZero
    ldy     #ID_BLANK
    NOP_IMM
.skipHiZero
    clv
    lda     DigitPtr,y
    sta     .scorePtrLst,x
    dex
    dex
    pla
    and     #$0f
    tay
    bne     .skipLoZero
    bvc     .skipLoZero
;    txa                        ;           display in case of score == 0
;    beq     .skipLoZero        ;            but who cares!? :-)
    ldy     #ID_BLANK
    NOP_IMM
.skipLoZero
    clv
    lda     DigitPtr,y
    sta     .scorePtrLst,x
    dex
    dex
    bpl     .loopScores
; prepare score kernel:
    sta     WSYNC
;---------------------------------------
    lda     #>DigitGfx          ; 2
    sta     .scorePtr0+1        ; 3
    sta     .scorePtr1+1        ; 3
    sta     .scorePtr2+1        ; 3
    sta     .scorePtr3+1        ; 3
    sta     .scorePtr4+1        ; 3
    sta     .scorePtr5+1        ; 3 = 20
    sta     GRP0                ; 3         clear shadow register of GRP1
    lda     #$f0|%11            ; 2
    sta     NUSIZ0              ; 3
    sta     NUSIZ1              ; 3
    sta     REFP0               ; 3
    sta     REFP1               ; 3
    sta     RESP0               ; 3 = 20    @40!
    sta     RESP1               ; 3
    sta     VDELP1              ; 3
    sta     HMCLR               ; 3
    sta     HMP0                ; 3
; /VerticalBlank
    jmp     DrawScreen
ContDrawScreen
;---------------------------------------------------------------
OverScan SUBROUTINE
;---------------------------------------------------------------
.loopCnt        = tmpVars
.xPos           = tmpVars+1
.maxLevel       = tmpVars+2

  IF NTSC_TIM
    lda     #36-2
  ELSE
    lda     #63
  ENDIF
    sta     TIM64T

    lda     #0
    ldx     #NUM_PLAYERS-1
.loopMax
    cmp     levelLst,x
    bcs     .nextMax
    lda     levelLst,x
.nextMax
    dex
    bpl     .loopMax
    sta     .maxLevel

    ldx     #NUM_PLAYERS-1
.loopPlayers
    stx     .loopCnt
    asl     cxSpriteBits
    bcc     .skipCXSprite
    lda     frameCnt
    lsr
    bcc     .checkEnemy
    lda     #X_BONUS_OFF
    cmp     xBonusLst,x
    beq     .checkEnemy
    sta     xBonusLst,x
    lda     levelLst,x
  REPEAT BONUS_SHIFT
    lsr
  REPEND
    and     #NUM_BONUS-1
    tay
    lda     BonusScore,y
    pha
    lda     BonusScoreHi,y
    tay
    pla
    jsr     AddScore
    jmp     .skipCXSprite

.checkEnemy
    lda     Pot2Bit,x       ; already dead?
    bit     enemyStates
    bne     .skipCXSprite   ;  yes, skip
;    ldy     powerTimLst,x   ; power-up enabled?
;    beq     .skipCXSprite   ;  no, skip
    ora     enemyStates      ; -> eyes
    sta     enemyStates
    lda     #ENEMY_PTS
    jsr     AddScoreLo
; set enemy escape direction:
    lda     xEnemyLst,x
    cmp     #SCW/2-4
    lda     enemyDirs
    bcc     .runLeft
    ora     Pot2Bit,x
    bcs     .setEnemyDir
.runLeft
    and     Pot2Mask,x
.setEnemyDir
    sta     enemyDirs
    lda     #0              ; disable power-up
    sta     powerTimLst,x
.skipCXSprite
    asl     cxPelletBits
    bcc     .skipCXPellet
; eat pellet:
    lda     xPlayerLst,x
    lsr
    lsr
    lsr
    tay
; clear pellet; TODO: when moving left, currently one pixel too early
; Y = x-pos, X = row
    txa
    clc
    adc     PfOffset,y
    tax
    lda     pfLst,x
    and     PfMask,y
    cmp     pfLst,x
    sta     pfLst,x
    beq     .skipCXPellet
    sty     .xPos
    ldx     .loopCnt
; check if power-up got eaten:
    lda     xPowerLst,x
    sec
    sbc     #6
    lsr
    lsr
    lsr
    eor     .xPos
    bne     .noPower
    sta     xPowerLst,x
    lda     #POWER_TIM      ; TODO: base timer on game speed
    sta     powerTimLst,x
    lda     #POWER_PTS
    NOP_W
.noPower
    lda     #PELLET_PTS     ; levelLst,x ?
    jsr     AddScoreLo

; check if all pellets cleared:
    lda     pf01LeftLst,x
    ora     pf20MiddleLst,x
    ora     pf12RightLst,x
    bne     .skipCXPellet
; increase speeds: (TODO: enemy speed)
; increase speed only for 1st player reaching next level:
    lda     levelLst,x
    cmp     .maxLevel
    bcc     .skipIncSpeed
    inc     .maxLevel
    lda     playerSpeed
;    clc
    adc     #DIFF_PL_SPEED-1
    cmp     #MAX_PL_SPEED+1
    bcs     .skipPlayerSpeed
    sta     playerSpeed
.skipPlayerSpeed
    lda     enemySpeed
    clc
    adc     #DIFF_EN_SPEED
    cmp     #MAX_EN_SPEED+1
    bcs     .skipEnemySpeed
    sta     enemySpeed
    sta     bonusSpeed
.skipEnemySpeed
.skipIncSpeed
  IF !TOP_SCORE
    stx     scorePlayerIdx
  ENDIF
    inc     levelLst,x
; reset pellets:
    jsr     ResetLine
; reset bonus:
    ldy     #X_BONUS_OFF
    lda     levelLst,x
    and     #BONUS_MASK     ; every 2nd/4th/8th level
    cmp     #BONUS_MASK
    bne     .disableBonus
    jsr     NextRandom
    and     Pot2Bit,x
    eor     bonusDirs
    sta     bonusDirs
    ldy     #SCW/2-4        ; start bonus at center
.disableBonus
    sty     xBonusLst,x
.skipCXPellet
;.nextPlayer
    ldx     .loopCnt
    dex
    bmi     .exitLoop
    jmp     .loopPlayers

.exitLoop

.waitTim
    lda     INTIM
    bne     .waitTim
; /OverScan
    jmp     MainLoop

;---------------------------------------------------------------
GameInit SUBROUTINE
;---------------------------------------------------------------
    lda     #%1
    sta     VDELP0
    sta     VDELBL

    lda     #%010000
    sta     CTRLPF
; setup initial board:
    ldx     #NUM_PLAYERS-1
.loopPf
    jsr     ResetLine
    lda     #SCW*1/2-16     ; at right of left power-ups
    sta     xPlayerLst,x
    lda     #SCW-8          ; right border
    sta     xEnemyLst,x
    lda     #X_BONUS_OFF    ; no bonus
    sta     xBonusLst,x
    dex
    bpl     .loopPf

; random initial directions:
    jsr     NextRandom
    sta     playerDirs
    lda     #INIT_PL_SPEED
    sta     playerSpeed
;    lsr
    lda     #INIT_EN_SPEED
    sta     enemySpeed
    sta     bonusSpeed
    rts
; /GameInit

;---------------------------------------------------------------
AddScoreLo SUBROUTINE
;---------------------------------------------------------------
    ldy     #0
AddScore
    sed
    clc
    adc     scoreLoLst,x
    sta     scoreLoLst,x
    tya
    adc     scoreHiLst,x
    sta     scoreHiLst,x
    cld
    rts

;---------------------------------------------------------------
ResetLine SUBROUTINE
;---------------------------------------------------------------
    lda     #$f5
    sta     pf01LeftLst,x
    lda     #$fa
    sta     pf20MiddleLst,x
    lda     #$ff
    sta     pf12RightLst,x
; position new power-up:
    jsr     NextRandom
    and     #$78
    cmp     #64
    bcc     .left
    adc     #32-1       ; skip 4 middle positions
.left
; @pellet 0..7, 12..19
;    clc
    adc     #6
    sta     xPowerLst,x
    rts

;---------------------------------------------------------------
NextRandom SUBROUTINE
;---------------------------------------------------------------
    lda     randomLo        ; 3
    lsr                     ; 2
  IF RAND16
    rol     randomHi        ; 5
  ENDIF
    bcc     .skipEor        ; 2/3
    eor     #EOR_RND        ; 2
.skipEor
    sta     randomLo        ; 3 = 16/17
  IF RAND16
    eor     randomHi        ; 3 = 19/20
  ENDIF
    rts
; /NextRandom

;    ALIGN_FREE 256
;
;;---------------------------------------------------------------
;SetXPos SUBROUTINE
;;---------------------------------------------------------------
;    sec
;    ldy     #0
;    sta     WSYNC
;;---------------------------------------
;    nop                     ; 2
;    nop     HMOVE           ; 3
;    sty     COLUBK          ; 3
;WaitObject:
;    sbc     #$0f            ; 2
;    bcs     WaitObject      ; 3/2
;
;  CHECKPAGE WaitObject
;    tay                     ; 2
;    lda     HMoveTbl-$f1,y  ; 5!
;    sta     RESP0,x         ; 4     @23..73!
;    sta     WSYNC
;;---------------------------------------
;;    sta     HMCLR
;    sta     HMP0,x          ; 4
;    rts
;; SetXPos


;===============================================================================
; R O M - T A B L E S (Bank 0)
;===============================================================================

    ALIGN_FREE_LBL 256, "Rom-Tables"

BonusPtr
; (Cherry, Strawberry, Orange, Apple, Melon, Galaxian, Bell, Key)
;    .byte   <GrapesGfx
    .byte   <CherryGfx, <StrawberryGfx, <OrangeGfx, <AppleGfx
    .byte   <MelonGfx, <GrapesGfx, <BananaGfx, <PearGfx
    CHECKPAGE BonusPtr
BonusColPtr
;    .byte   <GrapesCol
    .byte   <CherryCol, <StrawberryCol, <OrangeCol, <AppleCol
    .byte   <MelonCol, <GrapesCol, <BananaCol, <PearCol
    CHECKPAGE BonusColPtr

  IF COLOR_LINES
LineCols
    .byte   BLACK|$8, CYAN_GREEN|$8, PURPLE|$8, ORANGE|$8
    .byte   GREEN_YELLOW|$8, BLUE_CYAN|$8, MAUVE|$8, YELLOW|$8, WHITE
    CHECKPAGE LineCols
  ENDIF

Pot2Bit
    .byte   $01, $02, $04, $08, $10, $20, $40, $80
    CHECKPAGE Pot2Bit
Pot2Mask
    .byte   ~$01, ~$02, ~$04, ~$08, ~$10, ~$20, ~$40, ~$80

PlayerPtr
    .byte   <PlayerGfx0, <PlayerGfx0, <PlayerGfx1, <PlayerGfx1
    .byte   <PlayerGfx2, <PlayerGfx2, <PlayerGfx1, <PlayerGfx1
    CHECKPAGE PlayerPtr

;EnemyPtr
;    .byte   <EnemyGfx0, <EnemyGfx0, <EnemyGfx1, <EnemyGfx1
;    CHECKPAGE EnemyPtr ;

PlayerCol
    .byte   BLACK|$f, CYAN_GREEN|$f, PURPLE|$f, ORANGE|$f
    .byte   GREEN_YELLOW|$f, BLUE_CYAN|$f, MAUVE|$f, YELLOW|$f
    CHECKPAGE PlayerCol
EnemyColPtr
    .byte   <EnemyCol1, <EnemyCol2, <EnemyCol3, <EnemyCol0
    .byte   <EnemyCol1, <EnemyCol2, <EnemyCol3, <EnemyCol0
    CHECKPAGE EnemyColPtr

ScoreLums
; highlighted:
    .byte   $f8
    .byte   $f8
    .byte   $f8
    .byte   $fa
    .byte   $fc
    .byte   $fe
    .byte   $fe
    .byte   $fc
    .byte   $fa
    .byte   $f8
;; rounded:
;    .byte   $f8
;    .byte   $fa
;    .byte   $fc
;    .byte   $fc
;    .byte   $fc
;    .byte   $fc
;    .byte   $fc
;    .byte   $fc
;    .byte   $fe
;    .byte   $0e
;; chrome:
;    .byte   $fa
;    .byte   $f8
;    .byte   $f6
;    .byte   $f4
;    .byte   $f2
;    .byte   $fe
;    .byte   $fe
;    .byte   $fc
;    .byte   $fc
;    .byte   $fa

    CHECKPAGE ScoreLums

PfOffset
    ds      2, pf01LeftLst - pfLst
    ds      4, pf01LeftLst - pfLst
    ds      4, pf20MiddleLst - pfLst
    ds      2, pf20MiddleLst - pfLst
    ds      4, pf12RightLst - pfLst
    ds      4, pf12RightLst - pfLst

PfMask
    .byte   %11011111, %01111111
    .byte   %10111111, %11101111, %11111011, %11111110
    .byte   %11111101, %11110111, %11011111, %01111111
    .byte   %11101111, %10111111
    .byte   %10111111, %11101111, %11111011, %11111110
    .byte   %11111101, %11110111, %11011111, %01111111

; Bonus Scores:
; Pellet          10      1
; Power-Up        50      5
; Enemy          100     10
; Cherry         100     10
; Strawberry     300     30
; Orange         500     50        (aka Peach, Yellow Apple)
; Apple          700     70
; Melon         1000    100
; Grapes        2000    200
; Banana        3000    300
; Pear          5000    500
BonusScore
;    .byte   $10, $20, $30, $40, $50, $60, $70, $80
    .byte   $10, $30, $50, $70;, $00, $00, $00, $00
BonusScoreHi
    .byte   0,   0,   0,   0,   1,   2,   3,   5

    ALIGN_FREE_LBL 256, "DigitGfx"

DigitGfx
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

Blank
    ds      FONT_H, 0
  CHECKPAGE_DATA_LBL DigitGfx, "DigitGfx"

ColDiff0    ; Player sprite shading
    .byte   $8
    .byte   $6
    .byte   $6
    .byte   $4
    .byte   $4
    .byte   $4      ; immediate load in kernel!
    .byte   0       ; pellet area
    .byte   0       ; pellet area
    .byte   $4
    .byte   $4
    .byte   $4
    .byte   $2
    .byte   $2
    .byte   $0
    CHECKPAGE ColDiff0

    .byte   " Pac-Lines x 8 - Demo V"
    VERSION_STR
    .byte   " - (C) 2024 Thomas Jentzsch "

DigitPtr
    .byte   <Zero, <One, <Two, <Three, <Four
    .byte   <Five, <Six, <Seven, <Eight, <Nine
ID_BLANK = . - DigitPtr
    .byte   <Blank

HMoveTbl
; this is calculated with 1 cycle extra on access
; it MUST NOT be at the END of a page
    .byte   $60, $50, $40, $30, $20, $10, $00
    .byte   $f0, $e0, $d0, $c0, $b0, $a0, $90, $80
  IF <HMoveTbl >= $f1
    ECHO ""
    ECHO "HMoveTbl aligned wrong!"
    ECHO ""
    ERR
  ENDIF

    .byte   "JTZ"

PfColTbl = . - (GFX_H-POWER_H)/2
PowerStart
    .byte   0       ; = $08
    .byte   0
    .byte   $08
    .byte   $0c
    .byte   $04
    .byte   $0e
POWER_H = . - PowerStart
;    ds      (GFX_H-POWER_H)/2, 0        ; ball disabled anyway
    CHECKPAGE (. + (GFX_H-POWER_H/2))    ; but still must not cross a page!

BcdTbl
    .byte $00, $06, $12, $18, $24, $30, $36
;    .byte $42, $48, $54, $60, $66

     ALIGN_FREE_LBL 256, "SpriteGfx"

SpriteGfx
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
;    .byte   %00011100
;    .byte   %00111110
;    .byte   %00111110
;    .byte   %01111111
;    .byte   %01111111
;    .byte   %01111100
;    .byte   %01111000
;    .byte   %01111000
;    .byte   %01111100
;    .byte   %01101111
;    .byte   %01101111
;    .byte   %00111110
;    .byte   %00111110
;    .byte   %00011100

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
;    .byte   %00011100
;    .byte   %00111110
;    .byte   %00111110
;    .byte   %01111111
;    .byte   %01111100
;    .byte   %01111000
;    .byte   %01110000
;    .byte   %01110000
;    .byte   %01111000
;    .byte   %01101100
;    .byte   %01101111
;    .byte   %00111110
;    .byte   %00111110
;    .byte   %00011100

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

EnaBlTbl                ; can be overlapped with PearGfx
    ds      (GFX_H-POWER_H)/2, 0
;    ds      1, %10
;    ds      4, 0
    ds      6, %10
;    ds      POWER_H, %10
;    ds      (GFX_H-POWER_H)/2, 0
DisBlTbl
    ds      GFX_H, 0
    CHECKPAGE SpriteGfx

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
    .byte   RED|$6-2
    .byte   RED|$6-2
    .byte   RED|$8-2
    .byte   RED|$4-2
    .byte   RED|$a-2
    .byte   RED|$a-2
    .byte   0
    .byte   0
    .byte   RED|$a-2
    .byte   RED|$a-2
    .byte   RED|$c-2
    .byte   RED|$c-2
    .byte   RED|$e-2
    .byte   RED|$e-2
EnemyCol3
    .byte   ORANGE|$6
    .byte   ORANGE|$6
    .byte   ORANGE|$8
    .byte   ORANGE|$4
    .byte   ORANGE|$a
    .byte   ORANGE|$a
    .byte   0
    .byte   0
    .byte   ORANGE|$a
    .byte   ORANGE|$a
    .byte   ORANGE|$c
    .byte   ORANGE|$c
    .byte   ORANGE|$e
    .byte   ORANGE|$e
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
    .byte   RED|$4
    .byte   RED|$6
    .byte   RED|$8
    .byte   RED|$8
    .byte   RED|$8
    .byte   RED|$8
    .byte   RED|$8
    .byte   RED|$8
    .byte   RED|$8
    .byte   RED|$a
    .byte   RED|$a
    .byte   RED|$a
    .byte   RED|$c
    .byte   GREEN|$8
OrangeCol
    .byte   ORANGE|$6
    .byte   ORANGE|$8
    .byte   ORANGE|$a
    .byte   ORANGE|$a
    .byte   ORANGE|$a
    .byte   ORANGE|$a
    .byte   ORANGE|$a
    .byte   ORANGE|$a
    .byte   ORANGE|$c
    .byte   ORANGE|$c
    .byte   ORANGE|$c
    .byte   ORANGE|$e
    .byte   GREEN|$6
    .byte   GREEN|$8
AppleCol
    .byte   RED|$4
    .byte   RED|$6
    .byte   RED|$8
    .byte   RED|$8
    .byte   RED|$8
    .byte   RED|$8
    .byte   RED|$8
    .byte   RED|$8
    .byte   RED|$a
    .byte   RED|$a
    .byte   RED|$a
    .byte   RED|$c
    .byte   BEIGE|$6
    .byte   BEIGE|$6
MelonCol
    .byte   GREEN|$4
    .byte   GREEN|$6
    .byte   GREEN|$2
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
    .byte   YELLOW|$6
    .byte   YELLOW|$8
    .byte   YELLOW|$a
    .byte   YELLOW|$a
    .byte   YELLOW|$a
    .byte   YELLOW|$a
    .byte   0
    .byte   0
    .byte   YELLOW|$a
    .byte   YELLOW|$a
    .byte   YELLOW|$a
    .byte   YELLOW|$c
    .byte   YELLOW|$6
    .byte   YELLOW|$6
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
    CHECKPAGE ColorTbls

    ORG_FREE_LBL BASE_ADR | $ffc, "Vectors"
Vectors
    .word   Start
    .word   Start


;===============================================================================
; O U T P U T
;===============================================================================

    LIST OFF
    ECHO ""
    ECHO "*** Free RAM   :", [$100 - STACK_SIZE - NUM_TMPS - RAM_END]d, "bytes ***"
    ECHO "*** Free ROM   :", [FREE_TOTAL + DEBUG_BYTES]d, "bytes ***"
    ECHO ""
    ECHO "*** Debug bytes:", [DEBUG_BYTES]d, "bytes ***"

  IF SAVEKEY
    ECHO ""
    ECHO "*** SaveKey enabled! ***"
  ENDIF
  IF PLUSROM
    ECHO ""
    ECHO "*** PlusROM enabled! ***"
  ENDIF

