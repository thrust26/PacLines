; Pac-Lines x 8
;
; (C)2024 - Thomas Jentzsch

    processor 6502
  LIST OFF
    include vcs.h
  LIST ON

; Legend:
; - open
; o partially solved
; + solved
; x cancelled
; ? maybe

; BUGs:
; - #3 timer overflows
;   + high score display

; TODOs:
; - sound
;   + only human players
;   o- prios:
;     o channel 0: death, bonus, waka-waka (alternating "wa" and "ka")
;     + channel 1: enemy eaten, eyes, scared, siren (pitch based on game speed)
;   o countdown
;   ? start
;   ? game over sound
;   - new high score (player sound, 10x ding)
; - select after game over must not change variation
; o demo mode
;   ? automatic start
; ? larger power-up pellet
; o flicker
;   ? all 3 objects
;   + Enemy and Bonus?
;   ? Player and Enemies when Bonus arrives (player is never over a pellet)
;   x ghost color value boost
; o high score
;   + score and level
;   + determine
;   - play sound for new high score
;   + reset
;     + when game variation changes
;     + keep for same variation as high score (were to store?)
;   o display
;   - save (PlusROM, SaveKey)
;   - do not display "Ln. 0"
; ? pellets, wafers, dots...?
; ? wider enemies?
; ? deadly bonuses (mushrooms)
; o better player animation
;   + alive, based on pos
;   - dead, based on id, where to store??? (xBonusLst?)
;   ? remove ghost at death
; ? support old and new controls

; Ideas:
; - alternative theme
;   ? robots (cute, bad)
;     - collecting tools: screw, screwdriver, screw nut, wrench,
;             batteries, pliers, multi meter, drilling machine
;   ? Jawbreaker jaws and smilies + Fruits
;   ? animals
;   ? spaceships
;   ? cars (police?)
;   ? abstract
;   ? ZPH: Cats, Toys, Treats
;   ? ...
; - countdown with Pac-Man

; DONEs:
; + difficulty ramp up
;   + speeds
;     + enemies and players seperately
;     + by value
;     + enemies slower than players at start
;     + enemies accelerates faster than players
;     + maximum speed for players and enemies
;     + players reach maximum speed earlier than enemies
;   + different start levels (e.g. 1, 5, 9; displayed with starting level)
;   x globally
;   x individually
;   + mixed (levels individual, speed global)
;   + trigger
;     x time
;     x individual level
;     + global level (human players only)
; + AI
;   + enemies (following player)
;     + hunting: move to player (no wrapping)
;     + running: move to side away from player
;     + dead   : move to far away side
;     + follow further to the left
; + display
;   + score
;     + color gradients
;     + level
;     + displayed player number
;     x hex score stored, converted to BCD (up to 65,535 instead 9,999)
;     + display top score
;     x display score of player which changed level last
;     x display score of player which set new maximum level
; + bonus items
;   + based on current individual level
;   + when should they appear?
;     + either at start of new level (delayed?)
;     x or when power-up gets collected (disapear at next level)
;   + when shoud bonuses disappear?
;     x disappear at border
;     x when power-up gets eaten
;     + when level is finished
;     x never (when eaten or new bonus appears)
; + running enemies graphics/colors
; + align collecting animation with pellets (sync animation with position)
; + delay ghost/bonus collisions (at least 50% overlapping)
; + in AI row, dark/grey...
;   + ghost, blink, eyes
;   + bonus
;   + lines
;   x pellets & power pill
; + controls
;   + dectect QuadTari
;   + start game
;   + active player
;   + reverse direction
; + RESET, SELECT
; + game variations (2x4)
; + game states
;   + %00, select (before start and at end of game); TODO: level display
;   + %10, start; TODO: level display
;   + %11, running
;   + %01, over (all human players killed)
; + controls
;   x press/release to switch directions
;   + hold = left, release = right
; + replace "lvl" with "ln."
; + #2 sometimes sound after game over
; - #1 powerLst might be able to become $ff
; + demo mode
;   + when no player got activated during start
;   x stop with button press
; + AI computer players
;   x reverse direction if ghost comes too close:
;     dist to border opposite of enemy/player speed < dist of ghost/enemy speed
;     (dP/sP < dE/sE => dP*sE < dE*sP)
;   x simplyfied: reverse at 8 - dist enemy from its border
;   + move to middle pellets after power pill


;---------------------------------------------------------------
; Code structure
; OverScan
; - determine maximum human player level
; - handle player/pellet collisions
; - handle player/enemy collisions
; - update audio
;---------------------------------------
; VerticalBlank
; - check console switches
; - setup player, enemy & bonus speeds
; - move players, enemies and bonuses
; - update player directions
; - prepare display
; Note: This means that collisions are checked after display and before players
;       get moved

;===============================================================================
; A S S E M B L E R - S W I T C H E S
;===============================================================================

VERSION         = $0030
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
FRAME_LINES     = 1 ; (-12) draw white lines at top and bottom
TOP_SCORE       = 1 ; (-41) display top score (else player who last changed level)
LARGE_POWER     = 0 ; (+/-0) rectangular power up
BUTTON_DIR      = 1 ; (+6, +1 RAM) button press directly determines direction
KILL_AI         = 1 ; (-4) enemies can kill AI players

THEME_ORG       = 1
THEME_ALT_1     = 0 ; TODO
THEME_ALT_2     = 0 ; TODO
THEME_ALT_3     = 0 ; TODO


;===============================================================================
; C O L O R - C O N S T A N T S
;===============================================================================

FRAME_COL   = WHITE
AI_LUM      = 4


;===============================================================================
; G A M E - C O N S T A N T S
;===============================================================================

EOR_RND         = $bd           ; %10110100 ($9c, $b4, $bd, $ca, $eb, $fc)

SCW             = 160
PELLET_H        = 2
POWER_H         = 6
SPRITE_W        = 8

NUM_PLAYERS     = 8
NUM_VARIATIONS  = 8         ; TODO

POWER_TIM       = 140

NUM_BONUS       = 8
X_BONUS_OFF     = $ff
BONUS_SHIFT     = 2                 ; 2 = every 4th level
BONUS_MASK      = (1<<BONUS_SHIFT)-1

; Note: player has to be able to reach a most centered power-up. That's the 8th
; spot from left or right. Meanwhile the enemy has to move 12 spots, which is
; 50% more. So the speed difference should be a bit below 50%.
; | o o o o o o o O . . . . O o o o o o o o|

;INIT_PL_SPEED   = 80
;DIFF_PL_SPEED   = 4
;MAX_PL_SPEED    = 180
;INIT_EN_SPEED   = INIT_PL_SPEED-8
;DIFF_EN_SPEED   = DIFF_PL_SPEED+2       ; -> equal speed after 4 levels
;MAX_EN_SPEED    = 240                   ; 33% faster than player

DELTA_SPEED     = 140                   ; 40% delta; start: player d% faster, end: enemy d% faster
INIT_EN_SPEED   = 48
DIFF_EN_SPEED   = 7-1
MAX_EN_SPEED    = 255
INIT_PL_SPEED   = (INIT_EN_SPEED * DELTA_SPEED + 50) / 100    ; -> equal speed after 8 levels
DIFF_PL_SPEED   = DIFF_EN_SPEED - 2
MAX_PL_SPEED    = (MAX_EN_SPEED * 100 + DELTA_SPEED / 2) / DELTA_SPEED
    ECHO "      player vs enemy"
    ECHO "Speeds: ", [MAX_PL_SPEED]d, "vs", [MAX_EN_SPEED]d
    ECHO "Ranges:", [MAX_PL_SPEED*100/8]d, "vs", [MAX_EN_SPEED*100/12]d
    ECHO ""

PELLET_PTS      = 1
POWER_PTS       = 5
ENEMY_PTS       = 10

; gameState constants:
DIFF_MASK       = $03       ; game variation
BONUS_GAME      = $04       ; bonus game flag
VAR_MASK        = DIFF_MASK|BONUS_GAME
DEMO_MODE       = $08
QT_RIGHT        = $10       ; bit 5==0: left QuadTari; bit 4==1: right QuadTari
QT_LEFT         = $20
QT_MASK         = QT_LEFT|QT_RIGHT
GAME_SELECT     = $00
GAME_START      = $80
GAME_RUNNING    = $c0
GAME_OVER       = $40
GAME_MASK       = $c0

;Select -> press & release -> Start -> 5s -> Run -> Over -> 2s -> press & release -> Select

; 00 = select?
; 10 = game start, select active players
; 11 = game running
; 01 = Game over, display scores of human players

DEBOUNCE        = $80

NUM_TMPS        = 6
STACK_SIZE      = 6                 ; used in score & kernel row setup


;===============================================================================
; Z P - V A R I A B L E S
;===============================================================================

    SEG.U   variables
    ORG     $80

frameCnt        .byte               ; even: enemy drawn, odd: bonus drawn
randomLo        .byte
  IF RAND16 ;{
randomHi        .byte
  ENDIF ;}
gameState       .byte               ; MMrLDBVV  Mode, Right/Left QuadTari, Demo, Bonus, Variation
debounce        .byte               ; D.......  Debounce ; TODO: = powerTimLst?
;---------------------------------------
levelLst        ds  NUM_PLAYERS
;---------------------------------------
xPlayerLst      ds  NUM_PLAYERS             ; 32 bytes
xEnemyLst       ds  NUM_PLAYERS
xBonusLst       ds  NUM_PLAYERS
xPowerLst       ds  NUM_PLAYERS
;---------------------------------------
playerAI        .byte               ; 0 = human, 1 = AI
hiScoreVar      = playerAI
;---------------------------------------
resetLst        = .
pfLst           ds  NUM_PLAYERS*3           ; 24 bytes
pf01LeftLst     = pfLst
pf20MiddleLst   = pfLst+NUM_PLAYERS
pf12RightLst    = pfLst+NUM_PLAYERS*2
;---------------------------------------
; only used during GAME_RUNNING:
playerSpeed     .byte
playerSpeedSum  .byte
enemySpeed      .byte
enemySpeedSum   .byte
bonusSpeed      = enemySpeed        ; TODO: eliminate if bonus speed is always 50% of enemy speed
bonusSpeedSum   .byte
; reused:
lastButtons     = playerSpeed       ; BBBBBBBB  reused during GAME_SELECT & GAME_OVER
waitedOver      = playerSpeedSum    ; ......WW  reused during GAME_OVER
playerIdx       = enemySpeed        ; ....PPPP  reused during GAME_OVER (score displayed when game is over)
ignoredScores   = enemySpeedSum     ; IIIIIIII  reused for displaying alternating scores
nxtIgnoredScores= bonusSpeedSum
;---------------------------------------
; row bits:
  IF !BUTTON_DIR
buttonBits      .byte               ; 1 = pressed
  ENDIF
playerLeft      .byte               ; 1 = left, 0 = right
enemyLeft       .byte               ; 1 = left, 0 = right
bonusLeft       .byte               ; 1 = left, 0 = right
playerDone      .byte               ; 0 = alive, 1 = dead/score rolled
enemyEyes       .byte               ; 0 = alive, 1 = eyes
;---------------------------------------
powerTimLst     ds NUM_PLAYERS              ;  8 bytes
;---------------------------------------
audIdxLst       ds  2
audIdx0         = audIdxLst
audIdx1         = audIdxLst+1
;---------------------------------------
scoreLst        ds  NUM_PLAYERS*2           ; 16 bytes
scoreLoLst      = scoreLst
scoreHiLst      = scoreLst+NUM_PLAYERS
NUM_RESETS      = . - resetLst
hiScoreLst      ds  3
hiScoreLo       = hiScoreLst
hiScoreHi       = hiScoreLst+1
hiScoreLvl      = hiScoreLst+2
  IF !TOP_SCORE
scorePlayerIdx  .byte               ; index of displayed score's player
  ENDIF
countDown       = scoreLoLst        ; reused during GAME_START
;---------------------------------------
cxPelletBits    .byte               ; temporary
cxSpriteBits    .byte               ; temporary

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
      brk
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

PfOffset
    ds      2, pf01LeftLst   - pfLst
    ds      4, pf01LeftLst   - pfLst
    ds      4, pf20MiddleLst - pfLst
    ds      2, pf20MiddleLst - pfLst
    ds      4, pf12RightLst  - pfLst
    ds      4, pf12RightLst  - pfLst

PfMask
    .byte   %11011111, %01111111
    .byte   %10111111, %11101111, %11111011, %11111110
    .byte   %11111101, %11110111, %11011111, %01111111
    .byte   %11101111, %10111111
    .byte   %10111111, %11101111, %11111011, %11111110
    .byte   %11111101, %11110111, %11011111, %01111111

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

;    ds  10, 0   ; line kernel alignment (tight contraints!)

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
    iny                         ; 2
    sty     ENABL               ; 3
    sty     GRP1                ; 3
    sty     GRP0                ; 3
    sty     GRP1                ; 3
    sty     NUSIZ0              ; 3 = 17
;---------------------------------------
    sty     NUSIZ1              ; 3
    sty     VDELP1              ; 3
    sty     GRP1                ; 3
    ldx     #$ff                ; 2
    txs                         ; 2
  IF LARGE_POWER
    lda     #%100000            ; 2
  ELSE
    lda     #%010000            ; 2
  ENDIF
    sta     CTRLPF              ; 3 = 20
;---------------------------------------------------------------
.loopCnt    = tmpVars
.color0     = .loopCnt
.bonusFlag  = tmpVars+1
.lineCol    = tmpVars+2
.ptrCol0    = tmpVars+3
.ptrCol1    = tmpVars+5
.ptr0       = tmpVars+7         ; used as stack by lines drawing
.ptr1       = tmpVars+9         ;   same
.enaBl      = tmpVars+11        ;   same

    lda     frameCnt            ; 3
    lsr                         ; 2
    lda     #$ff                ; 2
    adc     #0                  ; 2
    sta     .bonusFlag          ; 3 = 10    0 = (potential) bonus frame, $ff = enemy frame
; TODO?: use for levelLst,x >> 2, and #$07

    lda     #>ColorTbls
    sta     .ptrCol0+1

  IF FRAME_LINES
    lda     #FRAME_COL
  ELSE
    lda     #0
  ENDIF
    sta     .lineCol

    ldx     #NUM_PLAYERS-1
.loopKernels                    ;           @65
    stx     .loopCnt            ; 3 =  3
  IF THEME_ORG
    lda     Pot2Bit,x           ; 4
    sta     WSYNC               ; 3 =  7    @71
;---------------------------------------
    sty     GRP1                ; 3         Y == 0!
; *** draw lines between players ***
; setup player sprite direction:
    ldy     #8                  ; 2
    and     playerLeft          ; 3
    beq     .right0             ; 2/3
    NOP_B                       ; 1
.right0
    dey                         ; 2
    sty     REFP0               ; 3 = 13
  ELSE ;THEME_ALT_1|THEME_ALT_2|THEME_ALT_3
    sta     WSYNC               ; 3 =  3
;---------------------------------------
    sty     GRP1                ; 3         Y == 0!
    SLEEP   13
  ENDIF

    SLEEP   16-13
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
LinesBranches
    bne     .positionBonus      ; 2/3
; position enemy:
    lda     xEnemyLst,x         ; 4
    adc     #X_OFS-1            ; 2
.contBonus                      ;   = 17    @56!
    pha                         ; 3         x-pos
    lda     #1                  ; 2
    pha                         ; 3         index
    lda     .lineCol            ; 3
    pha                         ; 3 = 14    color
; prepare power-up (not drawn from stack):
    lda     xPowerLst,x         ; 4         x-pos           BUG#1: this can become $ff!
    ldx     #4                  ; 2 =  6    index
;---------------------------------------
    ldy     #0                  ; 2         color   @01
;    clc                         ; 2
    adc     #X_OFS              ; 2
    sec                         ; 2
    bcs     .enterLoop          ; 3 =  9!   @08!

.positionBonus                  ;12
    adc     #X_OFS              ; 2
    bcc     .contBonus          ; 3 = 17

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
    sbc     #X_OFS              ; 2                 CF == 1!   BUG#1: this can become $ff!
.waitLeft
    sbc     #$0f                ; 2
    bcs     .waitLeft           ; 3/2
    CHECKPAGE LinesBranches
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
.exit                           ;           @02/04, CF == 0
TIM_0a
    ldx     .loopCnt            ; 3
    txs                         ; 2         store .loopCnt in SP
; setup player color pointer:
    ldy     #<HumanColMask      ; 2
    lda     gameState           ; 3         display colors in demo mode
    and     #GAME_MASK          ; 2
    eor     #GAME_START         ; 2         GAME_START?
    bne     .startMode          ; 2/3        no, color all sprites
    lda     playerAI            ; 3
    and     Pot2Bit,x           ; 4
    cmp     #1                  ; 2         also sets AI state
.startMode
    lda     LineCols,x          ; 4
    bcc     .brightLine         ; 2/3
    ldy     #<AIColMask         ; 2
    eor     #(LINE_LUM ^ AI_LUM); 2         -> lum == 2
.brightLine                     ;
    sta     .lineCol            ;   = ..35
    sty     .ptrCol0            ; 3 =  3
TIM_0b
    sta     WSYNC
;---------------------------------------
    sta     COLUBK              ; 3 =  3
;TIM_1a ; timers have problems with preceding WSYNCs!
; setup player sprite pointer:
    lda     PlayerCol,x         ; 4
    sta     .color0             ; 3
    lda     xPlayerLst,x        ; 4         animate in sync with position (= speed based)
    and     #7                  ; 2
    tay                         ; 2
    lda     playerLeft          ; 3
    and     Pot2Bit,x           ; 4
    bne     .playerLeft         ; 2/3
    iny                         ; 2         adjust animation when going right
.playerLeft
    lda     PlayerPtr,y         ; 4
    sta     .ptr0               ; 3 = 32/33

;; setup player sprite direction:
;    ldy     #8-1                ; 2
;    lda     playerLeft          ; 3
;    and     Pot2Bit,x           ; 4
;    beq     .right0             ; 2/3
;    iny                         ; 2
;.right0
;    sty     REFP0               ; 3 = 15/16 ;
;; constant timing code:
;    ldy     #7                  ; 2
;    lda     playerLeft          ; 3
;    and     Pot2Bit,x           ; 4
;    beq     .right0             ; 2/3
;    NOP_B                       ; 1
;.right0
;    iny                         ; 2
;    sty     REFP0               ; 3 = 17

; setup power-up pointer:
    ldy     #2                  ; 2
    lda     #0                  ; 2
TIM_1b  ; 17..18 (+6) cycles
    sta     WSYNC               ; 3 =  5
;---------------------------------------
    sta     HMOVE               ; 3
TIM_2a
    sta     COLUBK              ; 3
    lda     xPowerLst,x         ; 4
    bne     .showPower          ; 2/3
    dey                         ; 2
.showPower                      ;   =  9/10
    sty     .enaBl              ; 3 =  9
; draw enemy or bonus:
    bit     .bonusFlag          ; 3
    bmi     .drawEnemy          ; 2/3
    lda     xBonusLst,x         ; 4
    eor     #X_BONUS_OFF        ; 2
    beq     .drawEnemy          ; 2/3=13/14
; draw bonus:
    lda     levelLst,x          ; 4
  REPEAT BONUS_SHIFT
    lsr                         ; 2/4/6     every 2nd/4th/8th.. level
  REPEND                        ;   =  6..10
    and     #NUM_BONUS-1        ; 2
    tay                         ; 2
    lda     BonusPtr,y          ; 4
    sta     .ptr1               ; 3
    lda     BonusColPtr,y       ; 4
    ldy     .ptrCol0            ; 3
    cpy     #<AIColMask         ; 2         AI row?
    bne     .setColPtr1B        ; 2/3        no
    lda     #<GreyCol           ; 2
.setColPtr1B
    sta     .ptrCol1            ; 3
    ldy     #0                  ; 2 = 20
    beq     .setRefP1           ; 3 =  3

.drawEyes                       ;           @45
    ldy     #<GreyCol           ; 2
    bcs     .setColPtr1Eyes     ; 2/3       AI row
    ldy     #<EnemyEyesCol      ; 2
.setColPtr1Eyes
    sty     .ptrCol1            ; 3
    ldy     #<EnemyEyesGfx      ; 2
    bne     .setPtr1            ; 3

.powerOn                        ;           @51
    ldy     #<GreyCol           ; 2
    bcs     .setColPtr1x        ; 2/3       AI row
    ldy     #<EnemyColDark      ; 2
    cmp     #$50                ; 2
    bcs     .setColPtr1x        ; 2/3
    and     #$08                ; 2
    bne     .setColPtr1x        ; 2/3
    ldy     #<EnemyColBlink     ; 2
.setColPtr1x                    ;
    sty     .ptrCol1            ; 3 = 5..19 @56..70
    ldy     #<EnemyDarkGfx0     ; 2
; setup graphics:
    lda     xEnemyLst,x         ; 4             animate in sync with position (= speed based)
    and     #$04                ; 2
    bne     .setPtr1            ; 2/3
    ldy     #<EnemyDarkGfx1     ; 2
    bne     .setPtr1            ; 3 = 13

.drawEnemy                      ;
    lda     Pot2Bit,x           ; 4
    bit     enemyEyes           ; 3
    bne     .drawEyes           ; 2/3= 9/10
; setup colors:
    lda     powerTimLst,x       ; 4
    bne     .powerOn            ; 2/3= 6/7
; power off:                    ;
    ldy     #<GreyCol           ; 2
    bcs     .setPtrCol1         ; 2/3       AI row
    ldy     EnemyColPtr,x       ; 4
.setPtrCol1
    sty     .ptrCol1            ; 3 = 8/11
    ldy     #<EnemyGfx0         ; 2
    lda     xEnemyLst,x         ; 4
    and     #$04                ; 2
    bne     .setPtr1            ; 2/3
    ldy     #<EnemyGfx1         ; 2
.setPtr1
    sty     .ptr1               ; 3 =  3
; setup enemy reflection:
    ldy     #8-1                ; 2
    lda     enemyLeft           ; 3
    and     Pot2Bit,x           ; 4
    beq     .setRefP1           ; 2/3
    iny                         ; 2         moving left
.setRefP1
    sty     REFP1               ; 3 = 14
; setup some high-pointer:
    lda     #>PlayerGfx         ; 2
    sta     .ptr0+1             ; 3
    lda     #>EnemyGfx          ; 2
    sta     .ptr1+1             ; 3
    lda     #>ColorTbls         ; 2
    sta     .ptrCol1+1          ; 3
    ldy     #GFX_H-1            ; 2 = 15
.loopTop                        ;
    lda     (.ptr0),y           ; 5
    sta     GRP0                ; 3         VDELed!
    lda     EnaBlTbl,y          ; 4
    and     .enaBl              ; 3
    sta     ENABL               ; 3         VDELed!
    lax     (.ptrCol1),y        ; 5         overwrites X!
    lda     .color0             ; 3
    and     (.ptrCol0),y        ; 5
TIM_2b ; 108..145 (+6) cycles
    sta     WSYNC               ; 3 = 34
;---------------------------------------
    sta     COLUP0              ; 3
    stx     COLUP1              ; 3
    lda     (.ptr1),y           ; 5
    sta     GRP1                ; 3         @14
    lda     PfColTbl,y          ; 4
    sta     COLUPF              ; 3 = 21    @21
    dey                         ; 2
    cpy     #(GFX_H+PELLET_H)/2 ; 2
    bcs     .loopTop            ; 3/2=7/6
    SLEEP   26                  ;26
    sta     CXCLR               ; 3         early enough
    tsx                         ; 2 = 31    SP = .loopCnt
; 1st pellet line:              ;           @58!
    lda     (.ptr0),y           ; 5
    sta     GRP0                ; 3         VDELed!
    lda     (.ptr1),y           ; 5
    dey                         ; 2
    sta     GRP1                ; 3 = 18    @00!
;---------------------------------------
    lda     #$0c                ; 2                 PfColTbl
    SLEEP   2
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
    lda     #$08                ;                   PfColTbl
;    SLEEP   2
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
;    lda     .color0             ; 3
;    sbc     #$4                 ; 2 = 18    @75         HumanColMask,y (Y = 5)
;---------------------------------------
;    sta     COLUP0              ; 3 =  3    @02         currently same as above!
    lda     (.ptrCol1),y        ; 5
    sta     COLUP1              ; 3         @10
    stx     GRP1                ; 3 = 11    @13
    lda     #$02                ; 2
    sta     COLUPF              ; 3 =  5    @18
    sta     PF0                 ; 3         @21
    lda     #0                  ; 2
    sta     PF1                 ; 3         @26
    sta     PF2                 ; 3         @29
    dey                         ; 2 = 13
.loopBtm                        ;
    lda     (.ptr0),y           ; 5
    sta     GRP0                ; 3         VDELed!
    lda     EnaBlTbl,y          ; 4
    and     .enaBl              ; 3
    sta     ENABL               ; 3         VDELed!
    lax     (.ptrCol1),y        ; 5
    lda     .color0             ; 3
    and     (.ptrCol0),y        ; 5
    sta     WSYNC               ; 3 = 36
;---------------------------------------
    sta     COLUP0              ; 3
    stx     COLUP1              ; 3
    lda     (.ptr1),y           ; 5
    sta     GRP1                ; 3 = 14    @14
    lda     #$08                ; 2         bottom pellet row color
    sta     COLUPF              ; 3         @19
    dey                         ; 2
    bpl     .loopBtm            ; 3/2=10/9
    iny                         ; 2
    sty     GRP0                ; 3         VDELed!
    tsx                         ; 2         restore .loopCnt
    txa                         ; 2
    ldx     #$ff                ; 2         restore SP
    txs                         ; 2
    tax                         ; 2 = 15
    lda     CXP0FB              ; 3         (write: WSYNC)
    asl                         ; 2
    rol     cxPelletBits        ; 5
    lda     CXPPMM              ; 3         (write: COLUP1)
    asl                         ; 2
    rol     cxSpriteBits        ; 5
    dex                         ; 2
    bmi     .exitLoop           ; 2/3
    jmp     .loopKernels        ; 3 = 27

.exitLoop
    sta     WSYNC
;---------------------------------------
    sty     GRP1                ;           Y = 0
    ldx     #8
    sta     WSYNC
;---------------------------------------
.waitResBl
    dex
    bne     .waitResBl
    stx.w   RESBL
; draw bottom lines:
    lda     .lineCol
    sta     WSYNC
;---------------------------------------
    sta     COLUBK
    ldx     #2
    sta     WSYNC
;---------------------------------------
    sty     COLUBK
; prepare score kernel:
    sty     COLUPF              ; 3
    lda     #%100               ; 2
    sta     CTRLPF              ; 3
    sta     GRP0                ; 3         clear shadow register of GRP1
    lda     #$f0|%11            ; 2
    sta     NUSIZ0              ; 3
    sta     NUSIZ1              ; 3
    sta     REFP0               ; 3
    sta     REFP1               ; 3
    sta     VDELP1              ; 3
    sta     HMCLR               ; 3
    sta     HMP0                ; 3
    sta     RESP0               ; 3 = 20    @40!
    sta     RESP1               ; 3
    sta     ENABL
  IF FRAME_LINES
    sta     WSYNC
    lda     #FRAME_COL
    sta     WSYNC
;---------------------------------------
    sta     COLUBK
    sta     WSYNC
;---------------------------------------
    sty     COLUBK
  ENDIF
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
;---------------------------------------------------------------
; Detect QuadTari in left and right port
; Check if INPT0/2 get low after ~3 frames
    ldy     #$82
    sty     VBLANK          ; enable bit 7, dump ports
    sta     WSYNC
;---------------------------------------
.loopWaitQ
    sta     WSYNC
;---------------------------------------
    sta     VBLANK          ; disable bit 7, A = 0!
    dex
    bne     .loopWaitQ
    dey
    bmi     .loopWaitQ
; total: 3x256 = 768 scanlines = ~2.9 NTSC frames
; Right port
; - INPT2 = 1 && INPT3 = 1 -> Paddles
; - INPT2 = 0 && INPT3 = 1 -> QuadTari
; - INPT2 = 0 && INPT3 = 0 -> SaveKey
    lda     INTIM           ; randomize
    ora     #$01
    sta     randomLo
    lda     INPT0           ; 3
    asl                     ; 2
    lda     INPT2           ; 3
    eor     INPT3           ; 3
    ror                     ; 2         bit 7==0: left QuadTari; ; bit 6==1: right QuadTari
    lsr                     ; 2
    lsr                     ; 2
    and     #QT_MASK        ; 2
  IF GAME_SELECT
    ora     #GAME_SELECT
  ENDIF
    jmp     ContInitCart
;---------------------------------------------------------------

ContDrawScreen
;---------------------------------------------------------------
OverScan SUBROUTINE
;---------------------------------------------------------------
; - determine maximum human player level
; - handle player/pellet collisions
; - handle player/enemy collisions
; - update audio

.loopCnt        = tmpVars
.xPos           = tmpVars+1
.maxLevel       = tmpVars+2

  IF NTSC_TIM
    lda     #36-2+3                 ; allows up to 2304 cycles
  ELSE
    lda     #63
  ENDIF
    sta     TIM64T
TIM_OS                              ; ~ 2091 cycles

    bit     gameState
    bmi     .startRunningMode       ; GAME_RUNNING|GAME_OVER
.notRunning                         ; GAME_SELECT|GAME_START|GAME_OVER
    jmp     .skipGameRunning

.startRunningMode
    bvc     .notRunning             ; GAME_OVER
; *** determine maximum human player level ***
    ldy     #0
    ldx     #NUM_PLAYERS-1
.loopMax
    lda     playerAI                ; only human players increase speeds (except for demo mode)
    cmp     #$ff                    ; DEMO_MODE?
    beq     .demoMode
    and     Pot2Bit,x
    bne     .nextMax
.demoMode
    tya
    cmp     levelLst,x
    bcs     .nextMax
    ldy     levelLst,x
.nextMax
    dex
    bpl     .loopMax
    sty     .maxLevel
; ~200 cycles

    ldx     #NUM_PLAYERS-1
.loopPlayers
    stx     .loopCnt
;---------------------------------------
; *** handle player/pellet collisions ***
    asl     cxPelletBits
    bcs     .eatPellet
;  bcc     .eatPellet
.skipCXPelletJmp
    ldx     .loopCnt
    jmp     .skipCXPellet

.eatPellet
; eat pellet:
  IF 0
; software collisions might allow 1 frame earlier power pill eating but not as
; long as players are moved AFTER collision check
; currently software collisions are 1 frame delayed just as hardware collisions
    lda     playerLeft
    and     Pot2Bit,x
    cmp     #1
    lda     xPlayerLst,x
    and     #$07
    bcs     .leftDir
    cmp     #1
    NOP_W
.leftDir
    cmp     #3
    bne     .skipCXPelletJmp
  ENDIF

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
    beq     .skipCXPelletJmp            ; has no valid X!
    sty     .xPos
    ldx     .loopCnt
; check if power-up got eaten:
    lda     xPowerLst,x
    sec
  IF LARGE_POWER
    sbc     #6-1
  ELSE
    sbc     #6
  ENDIF
    lsr
    lsr
    lsr
    eor     .xPos
    bne     .noPower
    sta     xPowerLst,x
    lda     #POWER_TIM
    sta     powerTimLst,x
    lda     #POWER_PTS
    NOP_W
.noPower
    lda     #PELLET_PTS     ; levelLst,x ?
    jsr     AddScoreLo
    lda     playerAI        ; only human players make sound
    and     Pot2Bit,x
    bne     .skipWaka
; Waka-Waka alternates between WAKA und KAWA sounds:
    lda     audIdx0
    cmp     #BONUS_END-1    ; other sound playing?
    bcc     .updataWaka     ;  no, updata waka
    tay
    lda     AudF0Tbl,y      ; other sound at end?
    bne     .skipWaka       ;  yes, skip waka, else start with 0
.updataWaka
    cmp     #WAKA_START     ; currently kawa played?
    bcs     .playsKawa      ;  yes, switch from kawa
    cmp     #WAKA_MID
    bcs     .skipWaka
    adc     #WAKA_LEN * 3   ; switch from waKA to KAwa
    bne     .setWakaIdx

.playsKawa
    cmp     #KAWA_MID
    bcs     .skipWaka
    sbc     #WAKA_LEN - 1   ; switch from kaWA to WAka
.setWakaIdx
    sta     audIdx0
.skipWaka
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
    lda     playerAI        ; only human players increase speeds (except for demo mode)
    cmp     #$ff            ; DEMO_MODE?
    beq     .demoModeInc
    and     Pot2Bit,x
    bne     .skipIncSpeed2
.demoModeInc
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
  IF MAX_EN_SPEED - 1 + DIFF_EN_SPEED < $100
    cmp     #MAX_EN_SPEED
  ENDIF
    bcs     .skipEnemySpeed
    sta     enemySpeed
;    sta     bonusSpeed
.skipEnemySpeed
.skipIncSpeed
  IF !TOP_SCORE
    stx     scorePlayerIdx
  ENDIF
.skipIncSpeed2
    ldy     levelLst,x
    iny
    cpy     #100
    bcc     .incLevel
    ldy     #100 - (1<<BONUS_SHIFT)*NUM_BONUS ; reset level to e.g. 68
.incLevel
    sty     levelLst,x
; new line with random power-up & pellets:
    jsr     SetupPowerPellets
; reset bonus:
    ldy     #X_BONUS_OFF    ; disable bonus
    lda     gameState
    and     #BONUS_GAME
    beq     .skipBonus
    lda     levelLst,x
    and     #BONUS_MASK     ; every 4th level
    bne     .skipBonus
    jsr     NextRandom
    and     Pot2Bit,x
    eor     bonusLeft
    sta     bonusLeft
    ldy     #SCW/2-4        ; start bonus at center
.skipBonus
    sty     xBonusLst,x
.skipCXPellet
;---------------------------------------
; *** handle player/enemy collisions ***
    asl     cxSpriteBits
    lda     Pot2Bit,x
    and     playerDone
    bne     .skipCXSpriteJmp
; player alive, check:
    bcc     .skipCXSpriteJmp
    lda     frameCnt
    lsr
    bcc     .checkEnemy
    lda     #X_BONUS_OFF
    cmp     xBonusLst,x
    beq     .checkEnemy
; must overlap at least 4 pixels:
    lda     xPlayerLst,x
    sec
    sbc     xBonusLst,x
    adc     #4
    cmp     #4*2
    bcs     .skipCXSpriteJmp
    lda     #X_BONUS_OFF
    sta     xBonusLst,x
; play bonus eaten sound:
DEBUG2
    lda     playerAI
    and     Pot2Bit,x
    bne     .skipBonusSound
    lda     audIdx0
    cmp     #DEATH_END
    bcs     .skipBonusSound
    lda     #BONUS_START
    sta     audIdx0
    lda     #BONUS_VOL
    sta     AUDV0
.skipBonusSound
; add bonus points:
    lda     levelLst,x
  REPEAT BONUS_SHIFT
    lsr
  REPEND
    and     #NUM_BONUS-1
    tax
    lda     BonusScore,x
    ldy     BonusScoreHi,x
    ldx     .loopCnt
    jsr     AddScore
.skipCXSpriteJmp
    jmp     .skipCXSprite

.checkEnemy
    lda     Pot2Bit,x       ; already dead?
    bit     enemyEyes
    bne     .skipCXSpriteJmp;  yes, skip
; must overlap at least 4 pixels:
    lda     xPlayerLst,x
    sec
    sbc     xEnemyLst,x
    adc     #4
    cmp     #4*2
    bcs     .skipCXSpriteJmp

    lda     Pot2Bit,x
    ldy     powerTimLst,x   ; power-up enabled?
    bne     .killEnemy      ;  yes, kill enemy
; kill player:
    and     playerAI
  IF !KILL_AI
    bne     .skipCXSpriteJmp
  ELSE
    bne     .skipKillSound
  ENDIF
; player death sound:
DEBUG3
    lda     #DEATH_START
    sta     audIdx0
    lda     #DEATH_VOL
    sta     AUDV0
.skipKillSound
; TODO: disable ghost instead of eyes:
    lda     Pot2Bit,x
    ora     enemyEyes
    sta     enemyEyes
; disable player and check for game over:
    lda     Pot2Bit,x
    ora     playerDone
    sta     playerDone
  IF KILL_AI
    tay
    lda     gameState
    and     #DEMO_MODE
    bne     .wait4AllDone
    tya
    ora     playerAI
    NOP_B
.wait4AllDone
    tya
  ELSE
    ora     playerAI
  ENDIF
    eor     #$ff
    bne     .skipCXSprite
; game over, display scores:
    sta     lastButtons         ; A = 0
    sta     frameCnt
    sta     playerIdx
    sta     ignoredScores
    lda     #2
    sta     waitedOver
; check for new high score:
    lda     gameState
    and     #DEMO_MODE
    bne     .skipHiScore
    jsr     Get1stPlayerScore   ; get for largest score
    ldy     scoreLoLst,x        ; player in X
    lda     scoreHiLst,x
    cmp     hiScoreHi
    bcc     .skipHiScore
    bne     .setHiScore
    cpy     hiScoreLo
    bcc     .skipHiScore
.setHiScore
    sty     hiScoreLo
    sta     hiScoreHi
    lda     levelLst,x
    sta     hiScoreLvl
.skipHiScore
    lda     gameState
    eor     #GAME_RUNNING^GAME_OVER
    sta     gameState
    lda     #0
    sta     AUDV1
    jmp     .skipGameRunning

.killEnemy
    lda     Pot2Bit,x
    ora     enemyEyes        ; -> eyes
    sta     enemyEyes
    lda     playerAI
    and     Pot2Bit,x
    bne     .skipEatenSound
    lda     #EATEN_START
    sta     audIdx1
    lda     #$0c
    sta     AUDC1
    lda     #EATEN_VOL
    sta     AUDV1
.skipEatenSound
    lda     #ENEMY_PTS
    jsr     AddScoreLo
; set enemy escape direction:
    lda     xEnemyLst,x
    cmp     #SCW/2-4
    lda     enemyLeft
    bcc     .runLeft
    ora     Pot2Bit,x
    bcs     .setEnemyDir
.runLeft
    and     Pot2Mask,x
.setEnemyDir
    sta     enemyLeft
    lda     #0              ; disable power-up
    sta     powerTimLst,x
.skipCXSprite
;---------------------------------------
; loop
;.nextPlayer
    ldx     .loopCnt
    dex
    bmi     .exitLoop
    jmp     .loopPlayers
; ~1931 cycles for loop

.exitLoop
;************* SOUNDS ****************
; *** Enemy sounds ***
; enemy eaten sound?
    ldy     audIdx1
    cpy     #EYES_END           ; eaten or eyes sound playing?
    bcs     .contEnemySound     ;  yes, continue current sound
; enemy eyes?
    ldx     #EYES_IDX
    lda     playerAI            ; ignore AI lines
    eor     #$ff
    and     enemyEyes
    bne     .startEnemySound
; enemy scared?
    cpy     #SCARED_END         ; scared sound playing?
    bcs     .contEnemySound     ;  yes, continue current sound
    ldx     #NUM_PLAYERS-1
.loopPowerTim
    lda     playerAI            ; ignore enemy lines
    and     Pot2Bit,x
    bne     .nextPlayer
    lda     powerTimLst,x
    beq     .nextPlayer
    ldx     #SCARED_IDX
    bne     .startEnemySound

.nextPlayer
    dex
    bpl     .loopPowerTim
; siren sound?
    lda     playerAI            ; ignore AI lines
    cmp     #$ff                ; DEMO_MODE?
    beq     .stopSound1         ;  yes, no sound
    cpy     #SIREN_END          ; siren sound playing?
    bcs     .contEnemySound     ;  yes, continue current sound
    ldx     #SIREN_IDX
.startEnemySound
    lda     #$04                ; AudC1Tbl,x
    sta     AUDC1
    lda     AudV1Tbl,x
    sta     AUDV1
    ldy     AudStart1Tbl,x
.contEnemySound
    ldx     AudF1Tbl,y
    beq     .stopSound1
    cpy     #SIREN_START+1
    bcs     .noSiren
    lda     .maxLevel           ; increase pitch every 2nd level
    lsr
    eor     #$ff
    cmp     #-12-1              ; decrease AUDF1 by up to 10
    bcs     .addLevel
    lda     #-12
.addLevel
    adc     AudF1Tbl,y
    tax
.noSiren
    stx     AUDF1
    dey
    NOP_W
.stopSound1
    ldy     #0
    sty     audIdx1
.skipGameRunning
; 2133 cyles for loop

; *** Player sounds ***
    ldy     audIdx0
    cpy     #DEATH_END
    bcc     .notDeath
    tya
    sbc     #DEATH_END
    ldx     #12
.loopDeath
    dex
    dex
    sbc     #12
    bcs     .loopDeath
    tay
    txa
    adc     AudF0Tbl+DEATH_END-$100+12,y
    tax
    bcc     .contDeath

.notDeath
    ldx     AudF0Tbl,y           ; varies
    beq     .stopSound0
.contDeath
    lda     #$c
    sta     AUDC0
    dec     audIdx0
    stx     AUDF0
    inx
    beq     .muteSound0
    cpy     #BONUS_END
    bcs     .endSound0
    ldx     #WAKA_VOL
.muteSound0
.stopSound0
    stx     AUDV0
.endSound0
TIM_OE

.waitTim
    lda     INTIM
    bne     .waitTim
; /OverScan
;---------------------------------------------------------------
VerticalBlank SUBROUTINE
;---------------------------------------------------------------
; - check console switches
; - setup player, enemy & bonus speeds
; - move players, enemies and bonuses
; - update player directions
; - prepare display

    lda     #%1110              ; each '1' bits generate a VSYNC ON line (bits 1..3)
.loopVSync
    sta     WSYNC               ; 1st '0' bit resets Vsync, 2nd '0' bit exits loop
;---------------------------------------
    sta     VSYNC
    lsr
    bne     .loopVSync          ; branch until VSYNC has been reset

    dec     frameCnt

  IF NTSC_TIM
    lda     #44-1-3             ; allows up to 2460 cycles
  ELSE
    lda     #77
  ENDIF
    sta     TIM64T
TIM_VS                          ; ~2833 cycles

; *** check console switches ***
    lda     SWCHB
    and     #%11
    cmp     #%11
    bcs     .notSwitched
    lsr                         ; put RESET bit into carry
    lda     debounce            ; DEBOUNCE?
    bmi     .skipSwitches
    ora     #DEBOUNCE
    sta     debounce
    php                         ; remember RESET bit
    jsr     GameInit
    lax     gameState
    and     #~GAME_MASK
  IF GAME_SELECT
    ora     #GAME_SELECT
  ENDIF
    cmp     gameState
    beq     .alreadyInSelect
    stx     hiScoreVar          ; remember game variation of current high score
.alreadyInSelect
    sta     gameState
    plp                         ; restore RESET bit
    bcc     .contReset
; handle SELECT switch:
    and     #VAR_MASK           ; increase game variation
    tay
    iny
    cpy     #NUM_VARIATIONS
    bcc     .skipResetVar
    ldy     #0
.skipResetVar
    tya
    eor     gameState
    and     #VAR_MASK
    eor     gameState
    sta     gameState
    bpl     .skipSwitches
    DEBUG_BRK

.notSwitched
    lda     debounce
    and     #~DEBOUNCE
    sta     debounce
.skipSwitches
    bit     gameState
    bpl     .selectOverMode     ; GAME_OVER|GAME_SELECT
    jmp     .startRunningMode   ; GAME_START|GAME_RUNNING

;---------------------------------------------------------------
; check all buttons for pressed and released:
.tmpButtons = tmpVars

.selectOverMode
    lda     SWCHA
    ldy     #$82
    sta     WSYNC
;---------------------------------------
    sty     VBLANK
    and     #%11001100
    lsr
    lsr
    sta     .tmpButtons
    lda     #%11001100
    sta     WSYNC           ; wait at least one scanline
;---------------------------------------
    and     SWCHA
    ora     .tmpButtons     ; %76435410
    eor     #$ff
    tay
    eor     lastButtons     ; last buttons ^ new buttons
    sty     lastButtons
; released: new buttons != last buttons && buttons == 0 (all released)
; wait for button release & press to switch
;    bit     gameState
    bvc     .checkSelectMode
    beq     .skipRunningJmp
    tya
    bne     .skipRunningJmp
; GAME_OVER
    lda     waitedOver          ; waited for display of 1st score?
    bne     .skipRunningJmp     ;  no, continue
    lda     gameState           ;  yes, switch to GAME_SELECT
    eor     #GAME_OVER^GAME_SELECT

    sta     hiScoreVar
ContInitCart
    sta     gameState
    jsr     GameInit

.skipRunningJmp
    jmp     .skipRunning

.checkSelectMode
    beq     .skipRunningJmp
; check if selected game variation has been changed:
    lda     gameState
    eor     hiScoreVar
    and     #VAR_MASK
    beq     .skipResetHiScore
; if changed, reset high score:
    lda     #0
    sta     hiScoreLo
    sta     hiScoreHi
    sta     hiScoreLvl
.skipResetHiScore

;---------------------------------------------------------------
; wait for button release & press to switch to start
.contReset
    lda     gameState
    eor     #GAME_SELECT^GAME_START
    and     #~DEMO_MODE
    sta     gameState

    lda     #COUNT_START
    sta     countDown
    lda     #30
    sta     frameCnt

.tmpXPowerLst = scoreHiLst  ; reused during GAME_START
    ldx     #NUM_PLAYERS-1
.loopClear
    lda     #$00
    sta     pf01LeftLst,x
    sta     pf20MiddleLst,x
    sta     pf12RightLst,x
    lda     xPowerLst,x
    sta     .tmpXPowerLst,x
    dex
    bpl     .loopClear
    lda     #$ff
    sta     playerAI
    bne     .skipRunningJmp ; skip checking both QuadTaris this frame again

; falls  through

;---------------------------------------------------------------

.startMode
    ldx     #NUM_PLAYERS-1
.loopStart
; coloring:
; 1. not pressed, not human: fully dark row, no pellets
; 2. not pressed, human: full colors, show pellets
; 3. pressed, human: full colors, hide pellets and power-up (TODO)

; if human, show pellets and powe-up while no button is pressed:
    ldy     #0
    lda     playerAI
    and     Pot2Bit,x
    bne     .notHuman
    jsr     SetupPellets
    ldy     .tmpXPowerLst,x
.notHuman
    sty     xPowerLst,x
; check pressed buttons for activating players:
; validate if QuadTari controller is used in right...
    cpx     #4
    bcs     .checkButton
    cpx     #2
    lda     gameState
    bcs     .checkQTLeft
    and     #QT_RIGHT
    bne     .checkButton    ; left/right bits are set opposite!
    beq     .notPressed

.checkQTLeft
    and     #QT_LEFT
    bne     .notPressed
.checkButton
    lda     SWCHA
    and     ButtonBit,x
    bne     .notPressed
    lda     playerAI
    and     Pot2Mask,x
    sta     playerAI
; hide pellets and power-up while button pressed
    lda     #$00
    sta     pf01LeftLst,x
    sta     pf20MiddleLst,x
    sta     pf12RightLst,x
    sta     xPowerLst,x
.notPressed
; switch to 2nd QuadTari port set
    cpx     #4
    bne     .skip2ndSetA
    lda     #$82
    sta     WSYNC
;---------------------------------------
    sta     VBLANK
    sta     WSYNC           ; wait at least one scanline
;---------------------------------------
.skip2ndSetA
    dex
    bpl     .loopStart

    lda     frameCnt
    bne     .skipRunningJmp2
    lda     #WAKA_START
    sta     audIdx0
    lda     #FPS*3/4
    sta     frameCnt
    dec     countDown
    bne     .skipRunningJmp2
; switch to running state:
; restore power-up:
    ldx     #NUM_PLAYERS-1
.loopReset
    jsr     SetupPellets
    lda     .tmpXPowerLst,x     ; restore all power-pills
    sta     xPowerLst,x
    lda     #$00                ; clear hi score
    sta     .tmpXPowerLst,x
    dex
    bpl     .loopReset
; initialize demo mode & speeds:
    lda     playerAI
    cmp     #$ff                ; DEMO_MODE?
    lda     gameState           ; display colors in demo mode
    bcc     .notDemoMode
    ora     #DEMO_MODE
.notDemoMode
; switch to running mode
    eor     #GAME_START^GAME_RUNNING
    sta     gameState
    and     #DIFF_MASK
    tay
;    lda     #INIT_PL_SPEED
    lda     PlayerSpeeds,y
    sta     playerSpeed
;    lda     #INIT_EN_SPEED
    lda     EnemySpeeds,y
    sta     enemySpeed
;    sta     bonusSpeed
    lda     VarLevels,y
    ldx     #NUM_PLAYERS-1
.loopLevels
    sta     levelLst,x
    dex
    bpl     .loopLevels
.skipRunningJmp2
    jmp     .skipRunning

;---------------------------------------------------------------
.playerSpeed    = tmpVars
.enemySpeed     = tmpVars+1
.bonusSpeed     = tmpVars+2
.tmpSpeed       = tmpVars+3

.startRunningMode
    bvs     .runningMode
    jmp     .startMode          ; GAME_START

; *** setup player, enemy & bonus speeds ***
.runningMode
TIM_SPS ; 56..91 cycles (more to VSYNC?)
    lda     #0
    sta     .playerSpeed
    lda     playerSpeed
    asl
    rol     .playerSpeed
    adc     playerSpeedSum
    sta     playerSpeedSum
    bcc     .skipIncPlayer
    inc     .playerSpeed        ; +4
.skipIncPlayer
    lda     #0
    sta     .enemySpeed
    lda     enemySpeed
    asl
    rol     .enemySpeed
    adc     enemySpeedSum
    sta     enemySpeedSum
    bcc     .skipIncEnemy
    inc     .enemySpeed         ; +4
.skipIncEnemy
    lda     frameCnt            ; 3
    lsr                         ; 2
    bcc     .skipBonusSpeed     ; 2/3=  7/8
    lda     #0                  ; 2
    sta     .bonusSpeed         ; 3
    lda     bonusSpeed          ; 3
    asl                         ; 2
    rol     .bonusSpeed         ; 5
    adc     bonusSpeedSum       ; 3
    sta     bonusSpeedSum       ; 3
    bcc     .skipIncBonus       ; 2/3
    inc     .bonusSpeed         ; 5
.skipIncBonus                   ;   = 24/28
.skipBonusSpeed
TIM_SPE

; *** move players, enemies and bonuses ***
    ldx     #NUM_PLAYERS-1
.loopMove
    lda     playerDone
    and     Pot2Bit,x
    beq     .doMove
    jmp     .nextMove

.doMove
    lda     playerLeft
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
    lda     enemyEyes
    and     Pot2Bit,x
    beq     .alive
    asl     .tmpSpeed       ; eyes move at double enemy speed
; dead, eyes:
    and     enemyLeft       ; continue moving into old direction
    beq     .posSpeedEyes
    bne     .negSpeedEyes

.alive
; fix for wrap around at left border:
    lda     xPlayerLst,x
    cmp     #SCW-SPRITE_W    ; 152
    bcc     .normalPos
    lda     #SCW/2-SPRITE_W/2
    cmp     xEnemyLst,x
    jmp     .contSpecial

.normalPos
    lda     xEnemyLst,x
    cmp     xPlayerLst,x
.contSpecial
    lda     enemyLeft
    ldy     powerTimLst,x
    bcs     .negSpeedEnemy
    bne     .negSpeedEnemyTim
.posSpeedEnemyTim
    and     Pot2Mask,x
    sta     enemyLeft
.posSpeedEyes
    clc
    lda     xEnemyLst,x
    adc     .tmpSpeed
    cmp     #SCW-9+1
    bcc     .setXEnemy
    lda     #SCW-8
    bcs     .reviveEnemy

.negSpeedEnemy
    bne     .posSpeedEnemyTim
.negSpeedEnemyTim
    ora     Pot2Bit,x
    sta     enemyLeft
.negSpeedEyes
    sec
    lda     xEnemyLst,x
    sbc     .tmpSpeed
    bcs     .setXEnemy
    lda     #0
.reviveEnemy
    tay
    lda     enemyEyes
    and     Pot2Mask,x
    sta     enemyEyes
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
    lda     bonusLeft
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

;---------------------------------------------------------------
; *** update player directions ***
; check buttons:
    lda     playerAI
    and     Pot2Bit,x
    bne     .aiPlayer
  IF !BUTTON_DIR ;{
    lda     SWCHA
    and     ButtonBit,x
    bne     .skipPressed
    lda     Pot2Bit,x
    bit     buttonBits
    bne     .skipReverseDir
; reverse direction
    lda     playerLeft
    eor     Pot2Bit,x
    sta     playerLeft
    lda     buttonBits
    ora     Pot2Bit,x           ; set button bit
    bne     .setBits

.skipPressed
    lda     buttonBits
    and     Pot2Mask,x          ; clear button bit
.setBits
    sta     buttonBits
.skipReverseDir
    jmp     .skipAI
  ELSE ;}
    lda     SWCHA
    and     ButtonBit,x
    cmp     #1
    bcs     .moveRight
    bcc     .moveLeft
  ENDIF

.aiPlayer
.absXPlayer     = tmpVars+4
.absXEnemy      = tmpVars+5
; only ~2 random AI players/frame:
    txa
    eor     randomLo
    eor     frameCnt
    and     #$03
    bne     .skipAI
; no AI during eyes:
    lda     enemyEyes
    and     Pot2Bit,x
    bne     .skipAI
; different AI during earlier power mode:
    lda     xPlayerLst,x
    ldy     powerTimLst,x
    cpy     #POWER_TIM*1/5
    bcc     .noAIPower
; AI player has power
    cmp     xEnemyLst,x
    bcs     .moveLeft
    bcc     .moveRight

.noAIPower
; AI player has no power:
    cmp     xEnemyLst,x
    bcc     .enemyRight
; enemy left:
; TODO: avoid tmp vars
; (159 - xPlayer) * 1.5 - (159 - xEnemy) >= 0  ->  right
; 239 - xPlayer * 1.5 - 159 + xEnemy     >= 0
; 80 - xPlayer * 1.5 + xEnemy            >= 0
; xPlayer * 1.5 - 80 - xEnemy            <= 0  ->  right
    lsr
    adc     xPlayerLst,x
    sbc     xEnemyLst,x
    bcc     .moveRight
    sbc     #SCW/2+4        ; consider minimum overlap
    bcc     .moveRight
    cmp     #SCW-16-4       ; consider wrap around
    bcc     .skipAI
.moveLeft
    lda     playerLeft
    ora     Pot2Bit,x
    bne     .setPlayerLeft

.enemyRight
    lsr
    adc     xPlayerLst,x    ; xPlayer * 1.5 < xEnemy  ->  xPlayer * 1.5 - xEnemy < 0
    sbc     xEnemyLst,x
; same logic as in enemyLeft, but opposite directions:
    bcs     .moveLeft
    cmp     #-4             ; consider minimum overlap
    bcs     .moveLeft
    cmp     #-SCW+16        ; consider wrap around
    bcs     .skipAI
.moveRight
    lda     playerLeft
    and     Pot2Mask,x
.setPlayerLeft
    sta     playerLeft
.skipAI

.nextMove
; switch to 2nd QuadTari port set:
    cpx     #4              ; 2
    bne     .skip2ndSet     ; 3/2= 7/6
;    sta     WSYNC
;---------------------------------------
    lda     #$82            ; 2
    sta     VBLANK          ; 3         wait at least one scanline until next SWCHA read
;    sta     WSYNC
;---------------------------------------
.skip2ndSet
    dex
    bmi     .exitLoop
    jmp     .loopMove

.exitLoop
.skipRunning

;---------------------------------------------------------------
PrepareDisplay SUBROUTINE
;---------------------------------------------------------------
; Display:
; - GAME_RUNNING:
;   - 1st player's score and level (alternating)
; - GAME_OVER:
;   - nth player's score and level (alternating)
;   - high score
; - GAME_SELECT
;   - start level and bonus
;   - TODO: high score (if saved)
; - GAME_START
;   - count down

; ******************** Setup Score ********************
.maxLo          = tmpVars
.ignored        = tmpVars+1
.count          = tmpVars+2
.player         = tmpVars+3
.tmpIgnored     = tmpVars+4
.number         = tmpVars+5
.numberDigit    = tmpVars+6
;---------------------------------------
.scoreLst       = tmpVars
.scoreLo        = .scoreLst
.scoreMid       = .scoreLst+1
.scoreHi        = .scoreLst+2
;---------------------------------------
.scorePtrLst    = tmpVars           ; overlapping with .scoreLst!
.scorePtr0      = .scorePtrLst      ; lowest digit
.scorePtr1      = .scorePtrLst+2
.scorePtr2      = .scorePtrLst+4
.scorePtr3      = .scorePtrLst+6
.scorePtr4      = .scorePtrLst+8
.scorePtr5      = .scorePtrLst+10   ; highest digit
;---------------------------------------
.scoreCol       = cxPelletBits

    ldy     #ID_BLANK
    sty     .numberDigit
    ldy     #NUM_PLAYERS-1

    bit     gameState
    bpl     .notRunningMode
    jmp     .startRunningMode       ; GAME_START|GAME_RUNNING

.notRunningMode
    bvc     .selectMode             ; GAME_SELECT
;---------------------------------------------------------------
; GAME_OVER

.countHuman = .ignored
; if all players are AI (demo mode), show all 8 AI scores:
    lax     playerAI
    inx                             ; DEMO_MODE?
    beq     .demoMode
; count human players:
    ldy     #-1
    ldx     #NUM_PLAYERS-1
.loopCountHuman
    lsr
    bcs     .isAI
    iny
.isAI
    dex
    bpl     .loopCountHuman
.demoMode
    sty     .countHuman
; check timer and update game state counter:
    ldx     playerIdx
    lda     frameCnt
    bpl     .sameRank
    lda     #FPS*2
    sta     frameCnt
    lsr     waitedOver
    dex
    bpl     .skipReset
DEBUG4
    lda     playerAI
    cmp     #$ff
    bcc     .notDemoMode
    lda     #0
.notDemoMode
    sta     nxtIgnoredScores
    ldx     .countHuman
    inx                         ; display high score
.skipReset
    lda     nxtIgnoredScores
    sta     ignoredScores
    stx     playerIdx
.sameRank
; check for high score display:
    cpx     .countHuman
    beq     .notHighScore
    bcc     .notHighScore
    ldx     #NUM_PLAYERS
    stx     .player
    lda     #FPS
    cmp     frameCnt
;.displayHighScore
    lda     #ID_LETTER_H
    sta     .number
    lda     #ID_LETTER_I
    sta     .numberDigit
    lda     hiScoreLo
    ldy     hiScoreHi
    bcc     .contDisplayHighScore
    lda     hiScoreLvl
    bcs     .contDisplayHighLevel

.notHighScore
    stx     .count
; select n-largest score to display:
    lda     .countHuman
    sec
    sbc     .count
    sta     .number             ; displayed rank - 1
    lda     ignoredScores
;  lda     #0
    inc     .number
    jsr     GetNthPlayerScore
    sta     nxtIgnoredScores
    ldy     #ID_DOT
    sty     .numberDigit
    lda     #FPS
    bne     .contGameOver
;---------------------------------------------------------------
.selectMode
; display bonus mode and level (B.LvLL):
    iny                         ; Y = NUM_PLAYERS - 1
    sty     .player
;    lda     #FPS
;    cmp     frameCnt
;    bcc     .displayHighScore
    ldy     #ID_BLANK
    lda     gameState
    and     #BONUS_GAME
    beq     .noBonus
    ldy     #ID_BONUS
.noBonus
    sty     .number
    lda     gameState
    and     #DIFF_MASK          ; game variation diff
    tay
    lda     VarLevels,y
    bne     .contSelectMode
---------------------------------------------------------------
.startRunningMode
    bvs     .runningMode
.startMode
; display countdown (------)
    lda     #WHITE
    sta     .scoreCol
    ldy     countDown
    lda     StartLoIds-1,y
    sta     .scoreLo
    lda     StartMidIds-1,y
    sta     .scoreMid
    lda     StartHiIds-1,y
    eor     #(ID_POWER^ID_PELLET)<<4
; display countdown (.....C)
;    lda     countDown
;    sta     .scoreLo
;    lda     #0
;    sta     .scoreMid
;    lda     #ID_BLANK<<4|ID_BLANK
    bne     .contStartMode
;---------------------------------------------------------------
.runningMode
TIM_SCS ; 523..759 cycles
  IF TOP_SCORE
; determine player with maximum score:
    jsr     Get1stPlayerScore   ; get for largest score
  ELSE
    ldx     scorePlayerIdx      ; last scoring player
  ENDIF
    txa
    eor     #$07
    sta     .number             ; displayed player number - 1
    inc     .number
    lda     #$55                ; 2/3 score, 1/3 level
.contGameOver
    cmp     frameCnt
    lda     scoreLoLst,x
    ldy     scoreHiLst,x
.contDisplayHighScore
    sta     .scoreLo
    sty     .scoreMid
    bcc     .displayScore
; display level:
    lda     levelLst,x
.contDisplayHighLevel
.contSelectMode
    HEX2BCD
    cmp     #$10
    bcs     .largeLevel
    ora     #ID_BLANK<<4
.largeLevel
    sta     .scoreLo
  IF TOP_SCORE
    ldx     .player
  ELSE
    ldx     scorePlayerIdx
  ENDIF
    lda     #ID_LETTER_L<<4|ID_Letter_N
    sta     .scoreMid
.displayScore
    lda     PlayerCol,x
    sta     .scoreCol
    lda     .number         ; display bonus, player number or rank (game over)
    asl
    asl
    asl
    asl
    ora     .numberDigit
.contStartMode
    sta     .scoreHi
; setup score into score pointers:
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
    lda     LeftDigitPtr,y
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
    lda     RightDigitPtr,y
    sta     .scorePtrLst,x
    dex
    dex
    bpl     .loopScores
    lda     #>DigitGfx          ; 2         TODO: 10 bytes could be saved
    sta     .scorePtr0+1        ; 3
    sta     .scorePtr1+1        ; 3
    sta     .scorePtr2+1        ; 3
    sta     .scorePtr3+1        ; 3
    sta     .scorePtr4+1        ; 3
    sta     .scorePtr5+1        ; 3 = 20
TIM_SCE
; /VerticalBlank
TIM_VE
    jmp     DrawScreen

;---------------------------------------------------------------
Get1stPlayerScore SUBROUTINE
;---------------------------------------------------------------
; A = n-1
TIM_GNPDS   ; 376 cycles

.maxLo      = tmpVars
.ignored    = tmpVars+1
;.count      = tmpVars+2                do NOT remove! (or update other code too)
.player     = tmpVars+3
.tmpIgnored = tmpVars+4

; display AI scores in demo mode:
    lda     playerAI
    cmp     #$ff            ;           DEMO_MODE?
    beq     .demoMode
    NOP_W
.demoMode
    lda     #0
GetNthPlayerScore           ;           A = previous players with higher scores
    sta     .ignored        ; 3         ignore AI players
    lda     #0              ; 2
    sta     .maxLo          ; 3
    sta     .player         ; 3         in case there are no active players
    ldx     #NUM_PLAYERS-1  ; 2 = 16
.loopNthMax
    asl     .ignored        ; 5         already selected?
    bcs     .nextNthMax     ; 2/3        yes, skip
    cmp     scoreHiLst,x    ; 4
    bcc     .setNthMax      ; 2/3
    bne     .nextNthMax     ; 2/3
    ldy     scoreLoLst,x    ; 4         (cpy abs,x is no opcode)
    cpy     .maxLo          ; 3
    bcc     .nextNthMax     ; 2/3
.setNthMax
    lda     scoreHiLst,x    ; 4
    ldy     scoreLoLst,x    ; 4
    sty     .maxLo          ; 3
    stx     .player         ; 3         nth largest score player idx
.nextNthMax
    dex                     ; 2
    bpl     .loopNthMax     ; 3/2       12..38
    ldx     .player         ; 3
    lda     ignoredScores   ; 3
    ora     Pot2Bit,x       ; 4
; X = nth player, A = ignored
TIM_GNPDE
    rts

;---------------------------------------------------------------
GameInit SUBROUTINE
;---------------------------------------------------------------
    ldx     #NUM_RESETS-1
    lda     #0
.loopReset
    sta     resetLst,x
    dex
    bpl     .loopReset

    sta     AUDV0
    sta     AUDV1

; setup initial board:
    ldx     #NUM_PLAYERS-1
.loopPf
    jsr     SetupPowerPellets
    lda     #SCW*1/2-16     ; at right of left power-ups
    sta     xPlayerLst,x
    lda     #SCW-8-1        ; right border-1
    sta     xEnemyLst,x
    lda     #X_BONUS_OFF    ; no bonus
    sta     xBonusLst,x
    lda     #1
    sta     levelLst,x
    dex
    bpl     .loopPf

    stx     VDELP0
    stx     VDELBL

; random initial directions:
;    jsr     NextRandom
;    sta     playerLeft
; 1 randomly disabled player at the bottom:
;    jsr     NextRandom
;    and     #3              ; bottom 4 rows
;    tax
;    lda     Pot2Bit,x
;    lda     #$ff
;    sta     playerAI
;    sta     playerDone
;    sta     enemyEyes

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
    bcc     .setScore
  IF !KILL_AI
    lda     gameState
    and     #DEMO_MODE
    eor     #DEMO_MODE
    beq     .demoMode
  ENDIF
    lda     Pot2Bit,x       ; stop player
    ora     playerDone
    sta     playerDone
    lda     #$99            ; limit score to 9,999
    sta     scoreLoLst,x
.demoMode
.setScore
    sta     scoreHiLst,x
    cld
    rts

;---------------------------------------------------------------
SetupPowerPellets SUBROUTINE
;---------------------------------------------------------------
; position new power-up:
    jsr     NextRandom
    and     #$78
    cmp     #64
    bcc     .left
    adc     #32-1       ; skip 4 middle positions
.left
; @pellet 0..7, 12..19
;    clc
  IF LARGE_POWER
    adc     #6-1
  ELSE
    adc     #6
  ENDIF
    sta     xPowerLst,x
SetupPellets
    lda     #$f5
    sta     pf01LeftLst,x
    lda     #$fa
    sta     pf20MiddleLst,x
    lda     #$ff
    sta     pf12RightLst,x
    rts

;---------------------------------------------------------------
NextRandom SUBROUTINE
;---------------------------------------------------------------
    lda     randomLo        ; 3
    lsr                     ; 2
  IF RAND16 ;{
    rol     randomHi        ; 5
  ENDIF ;}
    bcc     .skipEor        ; 2/3
    eor     #EOR_RND        ; 2
.skipEor
    sta     randomLo        ; 3 = 16/17
  IF RAND16 ;{
    eor     randomHi        ; 3 = 19/20
  ENDIF ;}
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

    ALIGN_FREE_LBL 256, "DigitGfx"

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
  IF LARGE_POWER
    .byte   %00011000
    .byte   %00111100
    .byte   %00111100
    .byte   %00111100
    .byte   %00111100
    .byte   %00011000
  ELSE
    .byte   %00011000
    .byte   %00011000
    .byte   %00111100
    .byte   %00111100
    .byte   %00011000
    .byte   %00011000
  ENDIF
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
EnaBlTbl                ; can be overlapped with PearGfx
    ds      (GFX_H-POWER_H)/2, 0
;    ds      1, %10
;    ds      4, 0
;    ds      1, %10
    ds      POWER_H, %10
    ds      (GFX_H-POWER_H)/2, 0

    CHECKPAGE_DATA_LBL DigitGfx, "DigitGfx"

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

    .byte   " Pac-Lines x 8 - Demo V"
    VERSION_STR
    .byte   " - (C) 2024 Thomas Jentzsch "

  IF THEME_ORG
    include     "gfx_org.h"
  ENDIF
  IF THEME_ALT_1
    include     "gfx_alt.h"
  ENDIF

;    ALIGN_FREE_LBL 256, "PlayerCol"

PlayerCol ; align LineCols!
  IF NTSC_COL
    .byte   RED|$f, CYAN_GREEN|$f, PURPLE|$f, ORANGE|$f
    .byte   GREEN_YELLOW|$f, BLUE_CYAN|$f, MAUVE|$f, YELLOW|$f
;    .byte   GREEN_YELLOW|$f, CYAN|$f, VIOLET|$f, BROWN|$f
;    .byte   GREEN|$f, BLUE|$f, RED|$f, YELLOW|$f
  ELSE
    .byte   BLACK|$f, CYAN_GREEN|$f, PURPLE|$f, ORANGE|$f
    .byte   GREEN_YELLOW|$f, BLUE_CYAN|$f, MAUVE|$f, YELLOW|$f
  ENDIF
    .byte   WHITE   ; score display
    CHECKPAGE PlayerCol

EnemyColPtr
    .byte   <EnemyCol1, <EnemyCol2, <EnemyCol3, <EnemyCol0
    .byte   <EnemyCol1, <EnemyCol2, <EnemyCol3, <EnemyCol0
    CHECKPAGE EnemyColPtr

LINE_LUM    = $a
LineCols
  IF NTSC_COL
    .byte   RED|LINE_LUM, CYAN_GREEN|LINE_LUM, PURPLE|LINE_LUM, ORANGE|LINE_LUM
    .byte   GREEN_YELLOW|LINE_LUM, BLUE_CYAN|LINE_LUM, MAUVE|LINE_LUM, YELLOW|LINE_LUM, WHITE
  ELSE
    .byte   WHITE|LINE_LUM, CYAN_GREEN|LINE_LUM, PURPLE|LINE_LUM, ORANGE|LINE_LUM
    .byte   GREEN_YELLOW|LINE_LUM, BLUE_CYAN|LINE_LUM, MAUVE|LINE_LUM, YELLOW|LINE_LUM, WHITE
  ENDIF
    CHECKPAGE LineCols

LVL_0   = 1    ; all must not divide by 4!
LVL_1   = 7
LVL_2   = 14
LVL_3   = 22

VarLevels
    .byte   LVL_0, LVL_1, LVL_2, LVL_3
PlayerSpeeds
    .byte   INIT_PL_SPEED+DIFF_PL_SPEED*(LVL_0-1)
    .byte   INIT_PL_SPEED+DIFF_PL_SPEED*(LVL_1-1)
    .byte   INIT_PL_SPEED+DIFF_PL_SPEED*(LVL_2-1)
    .byte   INIT_PL_SPEED+DIFF_PL_SPEED*(LVL_3-1)
EnemySpeeds
    .byte   INIT_EN_SPEED+DIFF_EN_SPEED*(LVL_0-1)
    .byte   INIT_EN_SPEED+DIFF_EN_SPEED*(LVL_1-1)
    .byte   INIT_EN_SPEED+DIFF_EN_SPEED*(LVL_2-1)
    .byte   INIT_EN_SPEED+DIFF_EN_SPEED*(LVL_3-1)

Pot2Mask
    .byte   ~$01, ~$02, ~$04, ~$08, ~$10, ~$20, ~$40, ~$80

Pot2Bit
    .byte   $01, $02, $04, $08, $10, $20, $40, $80
    CHECKPAGE Pot2Bit

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
    .byte   $00
    .byte   $10, $30, $50, $70, $00, $00, $00
BonusScoreHi
    .byte   $05
    .byte   $00, $00, $00, $00, $01, $02, $03

ButtonBit
    .byte   %00000100
    .byte   %00001000
    .byte   %01000000
    .byte   %10000000
    .byte   %00000100
    .byte   %00001000
    .byte   %01000000
    .byte   %10000000
  CHECKPAGE ButtonBit


StartLoIds
    .byte   ID_BLANK<<4|ID_BLANK
    .byte   ID_BLANK<<4|ID_BLANK
;    .byte   ID_BLANK<<4|ID_BLANK
;    .byte   ID_BLANK<<4|ID_BLANK
;    .byte   ID_PELLET<<4|ID_BLANK
;    .byte   ID_PELLET<<4|ID_PELLET
StartMidIds
    .byte   ID_BLANK<<4|ID_BLANK
    .byte   ID_BLANK<<4|ID_BLANK
;    .byte   ID_PELLET<<4|ID_BLANK
;    .byte   ID_PELLET<<4|ID_PELLET
;    .byte   ID_PELLET<<4|ID_PELLET
;    .byte   ID_PELLET<<4|ID_PELLET
StartHiIds
    .byte   ID_PELLET<<4|ID_BLANK
    .byte   ID_PELLET<<4|ID_PELLET
    .byte   ID_PELLET<<4|ID_PELLET
    .byte   ID_PELLET<<4|ID_PELLET
    .byte   ID_PELLET<<4|ID_PELLET
    .byte   ID_PELLET<<4|ID_PELLET
COUNT_START = . - StartHiIds

    .byte   "QUADTARI"


LeftDigitPtr
    .byte   <Zero, <One, <Two, <Three, <Four
    .byte   <Five, <Six, <Seven, <Eight, <Nine
ID_BLANK = . - LeftDigitPtr
    .byte   <Blank
ID_PELLET = . - LeftDigitPtr
    .byte   <PelletChar
ID_POWER = . - LeftDigitPtr
    .byte   <PowerChar
ID_LETTER_L = . - LeftDigitPtr
    .byte   <Letter_L
ID_BONUS = . - LeftDigitPtr
    .byte   <BonusGfx
ID_LETTER_H = . - LeftDigitPtr
    .byte   <Letter_H

RightDigitPtr
    .byte   <Zero, <One, <Two, <Three, <Four
    .byte   <Five, <Six, <Seven, <Eight, <Nine
ID_BLANK = . - RightDigitPtr
    .byte   <Blank
ID_PELLET = . - RightDigitPtr
    .byte   <PelletChar
ID_DOT = . - RightDigitPtr
    .byte   <DotChar
ID_Letter_N = . - RightDigitPtr
    .byte   <Letter_N
ID_LETTER_I = . - RightDigitPtr
    .byte   <Letter_I

    ALIGN_FREE_LBL 256, "PfColTbl"

PfColTbl = . - (GFX_H-POWER_H)/2 - 4
PowerStart
;    .byte   0       ; = $08
;    .byte   0       ; = $02
;    .byte   0       ; = $08 bottom pellet line
;    .byte   0       ; = $0c top pellet line
    .byte   $04
    .byte   $0e
POWER_H = . - PowerStart + 4
;    ds      (GFX_H-POWER_H)/2, 0        ; ball disabled anyway
    CHECKPAGE (. + (GFX_H-POWER_H/2))    ; but still must not cross a page!

BcdTbl
    .byte $00, $06, $12, $18, $24, $30, $36
;    .byte $42, $48, $54, $60, $66

    .byte   "JTZ"

; Dintari PacMan 8K:
; Siren : V=8; C=4; F=0e..1b, 1a..0f
; Scared: V=8; C/F = c/12, 4/1b, 12, 0d, 0a, 08, 07, 06
; Eyes  : V=8; C=4; F=06, 07, 08, 09, 0a, 0b, 0c, 0d, 0f, 11, 13, 15, 18, 1b, 1f

; Fruit : V=e; C=C; F=09, 0a, 0b, 0d, 0f, 12, 15, 18, 1d, 1f, 1d, 1b, 19, 17, 15, 12, 0f, 0d, 0b, 0a, 09, 08
; Death : V=e; C=4; F=06, 07, 08, 09, 0b, 0d, 10, 14, 1c
;   C=C; F=0d, 1b, -, C=4; F=06, 07, 08, 09, 0b, 0d, 10, 14, 1c
; C=4; F=

; C=4; F=
; 11, 10, 0f, 0e, 0d, 0e, 0f, 10, 11, 12, 13, 14
; 13, 12, 11, 10, 0f, 10, 11, 12, 13, 14, 15, 16
; 15, 14, 13, 12, 11, 12, 13, 14, 15, 16, 17, 18
; 17, 16, 15, 14, 13, 14, 15, 16, 16, 18, 19, 1a
; 19, 18, 17, 16, 15, 16, 17, 18, 19, 1a, 1b, 1c
; 1b, 1a, 19, 18, 17, 18, 19, 1a, 1b, 1c, 1d, 1e
; 1d
; 1d + bang/bang

WAKA_VOL    = $8    ; pellet eaten
BONUS_VOL   = $c    ; bonus eaten
DEATH_VOL   = $c    ; player killed

SIREN_VOL   = $4    ; enemy hunting
SCARED_VOL  = $6    ; enemy scares
EYES_VOL    = $6    ; enemy eyes
EATEN_VOL   = $a    ; enemy eaten

; *** Player sounds ***
AudF0Tbl
;WAKA_END = . - AudF0Tbl
    .byte   0
    ds      3, $ff
    .byte   $18, $11, $0e, $0b, $0a
WAKA_LEN = . - AudF0Tbl
WAKA_MID = . - AudF0Tbl                     ; = 9
    ds      4, $ff
    .byte   $0a, $0b, $0e, $11, $18
WAKA_START = . - AudF0Tbl - 1               ; = 18
;KAWA_END = . - AudF0Tbl
    .byte   0
    ds      3, $ff
    .byte   $0a, $0b, $0e, $11, $18
KAWA_MID = . - AudF0Tbl                     ; = 9
    ds      4, $ff
    .byte   $18, $11, $0e, $0b, $0a
KAWA_START = . - AudF0Tbl - 1               ; = 18
BONUS_END = . - AudF0Tbl + 1
    .byte   0
    .byte   $08, $09, $0a, $0b, $0d, $0f, $12, $15, $17, $19, $1b
    .byte   $1d, $1f, $1d, $18, $15, $12, $0f, $0d, $0b, $0a, $09
BONUS_START = . - AudF0Tbl - 1
DEATH_END = . - AudF0Tbl + 1
    .byte   0
;final sounds missing here...
;    .byte   $1e, $1d, $1c, $1b, $1a, $19, $18, $17, $18, $19, $1a, $1b
;    .byte   $1c, $1b, $1a, $19, $18, $17, $16, $15, $16, $17, $18, $19
;    .byte   $1a, $19, $18, $17, $16, $15, $14, $13, $14, $15, $16, $17
;    .byte   $18, $17, $16, $15, $14, $13, $12, $11, $12, $13, $14, $15
;    .byte   $16, $15, $14, $13, $12, $11, $10, $0f, $10, $11, $12, $13
    .byte   $14, $13, $12, $11, $10, $0f, $0e, $0d, $0e, $0f, $10, $11
DEATH_START = . + 5 * 12 - AudF0Tbl - 1
; 339 bytes -> 370 bytes

; *** Enemy Sounds ***
SIREN_IDX   = 0
SCARED_IDX  = 1
EYES_IDX    = 2
;EATEN_IDX   = 3

AudStart1Tbl
    .byte   SIREN_START, SCARED_START, EYES_START;, EATEN_START
AudV1Tbl
    .byte   SIREN_VOL, SCARED_VOL, EYES_VOL;, EATEN_VOL
;AudC1Tbl
;    .byte   $04, $04, $04;, $0c
AudF1Tbl
SIREN_END = . - AudF1Tbl + 1
    .byte   0
    .byte   $1b+4, $1a+4, $19+4, $18+4, $17+4, $16+4, $15+4, $14+4, $13+4, $12+4, $11+4, $10+4, $0f+4
    .byte   $0e+4, $0f+4, $10+4, $11+4, $12+4, $13+4, $14+4, $15+4, $16+4, $17+4, $18+4, $19+4, $1a+4
SIREN_START = . - AudF1Tbl - 1
SCARED_END = . - AudF1Tbl + 1
    .byte   0
    .byte   $1f, $1b, $12, $0d, $0a, $08, $07, $06
SCARED_START = . - AudF1Tbl - 1
EYES_END = . - AudF1Tbl + 1
    .byte   0
    .byte   $08, $07, $06
    .byte   $1f, $1b, $18, $15, $13, $11, $0f, $0d, $0c, $0b, $0a, $09
EYES_START = . - AudF1Tbl - 1
;EATEN_END = . - AudF1Tbl + 1
    .byte   0
    .byte   $05, $06, $07, $08
    .byte   $09, $0a, $0b, $0d, $0f, $12, $15, $1d, $1f
EATEN_START = . - AudF1Tbl - 1

    ORG_FREE_LBL BASE_ADR | $ffc, "Vectors"
Vectors
    .word   Start
    .word   VERSION


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

