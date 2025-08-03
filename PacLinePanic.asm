; Pac-Line Panic
;
; (C)2025 - Thomas Jentzsch

    processor 6502
  LIST OFF
    include vcs.h
  LIST ON

; Legend:
; - open
; o partially done
; + done
; x cancelled
; ? maybe

; BUGs:
; - #6 why is extra WSYNC required for Stella? (Stella bug?)

; TODOs:
; - AI has problems with 1st pellet left of center
; - PAL color checks
; - update version number
; o high scores laden
; - high scores jedes Mal senden
; - ...

; Ideas:
; x alternative theme (for cart release)
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
; ? pellets, wafers, dots...?
; x wider enemies?
; x deadly bonuses (mushrooms)
; x countdown with player graphics eating pellets
; x high score screen (787 cycles per line?)
; x 16-bit random

; DONEs:
; + difficulty ramp up
;   + speeds
;     + enemies and players seperately
;     + by value
;     + enemies slower than players at start
;     + enemies accelerate faster than players
;     + maximum speed for players and enemies
;     + players reach maximum speed earlier than enemies
;   + different start lines (e.g. 1, 5, 9; displayed with starting line)
;   x globally
;   x individually
;   + mixed (lines individual, speed global)
;   + speed defined by
;     x time
;     x individual line
;     + global line (human players only)
; + AI
;   + enemies (following player)
;     + hunting: move to player (no wrapping)
;     + running: move to side away from player
;     + dead   : move to far away side
;     + follow further to the left
; + display
;   + score
;     + color gradients
;     + line
;     + displayed player number
;     x hex score stored, converted to BCD (up to 65,535 instead 9,999)
;     + display top score
;     x display score of player which changed line last
;     x display score of player which set new maximum line
;     + do not display "Ln. 0"
;     + display player number alterating with rank number (solves close colors problem)
; + bonus items
;   + based on current individual line
;   + when should they appear?
;     + either at start of new line (delayed?)
;     x or when power-up gets collected (disapear at next line)
;   + when shoud bonuses disappear?
;     x disappear at border
;     x when power-up gets eaten
;     + when line is finished
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
;   + %00, select (before start and at end of game)
;   + %10, start
;   + %11, running
;   + %01, over (all human players killed)
; + controls
;   + press/release to switch directions
;   + hold = left, release = right
;   + diffiulty switch changes controls
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
; + better player animation
;   + alive, based on pos
;   + dead, based on power timer
;   + remove ghost at death
; + 1st select after game over must not change variation
; + fire after game over should directly start new game (like RESET)
; + start with bonus game variations
; + display high score during game select
; + high score
;   + score and line
;   + determine
;   + play sound for new high score (when reached and was not zero before)
;   + reset
;     + when game variation changes
;     + keep for same variation as high score
;   + save (PlusROM only)
; + #3 timer overflows
;   + caused by high score display
;   + caused by too many player/enemy collisions in the same frame
;   + caused by too much AI logic in the same frame
;   Ideas:
;   + move code into VSYNC
;   + do not calc from scratch every frame (.maxLine , firstPlayer)
;   + optimize code
;   + check timer and abandon early
; + sound
;   + only human players
;   + prios:
;     + channel 0: death, bonus, waka-waka (alternating "wa" and "ka")
;     + channel 1: enemy eaten, eyes, scared, siren (pitch based on game speed)
;   + countdown
;   x start
;   x game over sound
;   + new high score (player sound, 10x ding)
; + #4 game starts with line not score
; + #5 AI players are not dying completely at end of game
; x larger power-up pellet
; + debounce uses only 1 bit, saves 6 bytes
; + extend score display intervals
; + different font (Pac-Man)
; + fire buttun aborts demo and resets game
; + support old and new controls (left difficulty)
; + demo mode
;   + repeating after score loop
;   + sound
;   x automatic start
; + increased scores for later lines (must not overflow!)
;   + line div 4 is OK (up to 8976 points at line 44)
;   + multiply with BCD is slow, reduce CPU time in overscan:
;     + pellet collision check for only 50% players
;     x hiscore check only for 1 or 2 players/frame (??? cycles at game start)
;     x powertim counter variable (for SCARED sound) (~200 cylces)
; + merge Left- and RightDigitPtr
; + flicker
;   x all 3 objects
;   + Enemy and Bonus?
;   x Player and Enemies when Bonus arrives (player is never over a pellet)
;   x ghost color value boost
; x check both (enemy & bonus) collisions every frame
; + add powerTim counter (optional)
; + only highest bonus from line 32 on
; + #7 score display after demo mode starts wrong
; + PlusCart tests
; x replace top fruits with original Pac-Man items: Galaxian, Bell, Key
; + load high score from PlusROM backend:
;   x check and set request active flag
;   + work if no PlusROM
;   + initial loading (variation 0)
; - #8 fix high scores loading, use delay (wait 1s after SELECT)


;---------------------------------------------------------------
; *** Code Structure ***
; OverScan
; - determine maximum human player line
; - handle player/pellet collisions
; - handle player/enemy collisions
; - update audio
;---------------------------------------
; DrawScreen
; - score kernel
; - 8 x game kernel
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

GAME_NAME       = "Pac-Line Panic"
VERSION         = $0110
BASE_ADR        = $f000     ; 4K

  IFNCONST TV_MODE ; manually defined here
NTSC            = 1
PAL60           = 0
PAL50           = 0 ; not supported!
  ENDIF

    include tv_modes.h

ILLEGAL         = 1
DEBUG           = 1

SAVEKEY         = 0 ; (-~220) support high scores on SaveKey (too many bytes)
PLUSROM         = 1 ; (-34) save current variation's high score to PlusStore HSC backend
PLUSROM_LOAD    = 1 ; (-52) load current variation's high score from PlusStore HSC backend
COPYRIGHT       = 0 ; (-40/41) add copyright notice into code
COPYRIGHT_SHORT = 0 ; (-21/22) add short copyright notice into code
RAND16          = 0 ; (-3, -1 RAM) 16-bit random values
FRAME_LINES     = 1 ; (-12) draw white lines at top and bottom
SWITCH_DIR      = 1 ; (-28, -1 RAM)
BUTTON_DIR      = 1 ; (-4) button press directly determines direction
                    ; (-5, if both enabled
BIG_MOUTH       = 1 ; (0) bigger mouth for player
HISCORE_DING    = 1 ; (-99) 10 dings at new high score
EXTEND_COUNTDOWN= 1 ; (-12) extends countdown with each new activated player
LONG_DEATH      = 1 ; (-??) = 0 is still TODO
LIMIT_LINE      = 0 ; (-9) limit line to 99 (doubtful that this can ever be reached)
DEMO_SOUND      = 1 ; (-12/15) play sounds during demo mode
MULT_SCORE      = 1 ; (-25) multiply scores by line/4
POWER_CNT       = 0 ; (-6) count active power-ups (saves some cycles if required)
MAX_BONUS       = 1 ; (-12) only highest bonus item(s) after line 32

THEME_ORG       = 1
THEME_ALT_1     = 0 ; TODO
THEME_ALT_2     = 0 ; TODO
THEME_ALT_3     = 0 ; TODO


;===============================================================================
; C O L O R - C O N S T A N T S
;===============================================================================

FRAME_COL       = WHITE
AI_LUM          = 4


;===============================================================================
; G A M E - C O N S T A N T S
;===============================================================================

EOR_RND         = $bd               ; %10110100 ($9c, $b4, $bd, $ca, $eb, $fc)

SCW             = 160
PELLET_H        = 2
POWER_H         = 6
SPRITE_W        = 8

NUM_PLAYERS     = 8
NUM_VARIATIONS  = 8                 ; TODO (8|4... saves 7 bytes)

POWER_TIM       = 140

NUM_BONUS       = 8                 ; enough for 32 lines
X_BONUS_OFF     = $ff
BONUS_SHIFT     = 2                 ; 2 = every 4th line
BONUS_MASK      = (1<<BONUS_SHIFT)-1

; Note: player has to be able to reach a most centered power-up. That's the 8th
; spot from left or right. Meanwhile the enemy has to move 12 spots, which is
; 50% more. So the speed difference should be a bit below 50%.
; | o o o o o o o O . . . . O o o o o o o o|

DELTA_SPEED     = 140                   ; 40% delta; start: player d% faster, end: enemy d% faster
INIT_EN_SPEED   = 48
DIFF_EN_SPEED   = 6
MAX_EN_SPEED    = 255                   ; reached after 35 lines
INIT_PL_SPEED   = (INIT_EN_SPEED * DELTA_SPEED + 50) / 100    ; -> equal speed after ~9 lines
DIFF_PL_SPEED   = DIFF_EN_SPEED - 2                                     ; = 4
MAX_PL_SPEED    = (MAX_EN_SPEED * 100 + DELTA_SPEED / 2) / DELTA_SPEED  ; reached after 29 lines
;    ECHO "      player vs enemy"
;    ECHO "Speeds: ", [MAX_PL_SPEED]d, "vs", [MAX_EN_SPEED]d
;    ECHO "Ranges:", [MAX_PL_SPEED*100/8]d, "vs", [MAX_EN_SPEED*100/12]d
;    ECHO ""

PELLET_PTS      = 1
POWER_PTS       = 5
ENEMY_PTS       = 10

SCORE_TIM       = FPS * 3 / 2

; gameState constants:
DIFF_MASK       = $03       ; game variation
NO_BONUS_GAME   = $04       ; no bonus game flag
VAR_MASK        = DIFF_MASK|NO_BONUS_GAME
  IF HISCORE_DING
HISCORE_DINGED  = $08
  ENDIF
QT_RIGHT        = $10       ; bit 5==0: left QuadTari; bit 4==1: right QuadTari
QT_LEFT         = $20
QT_MASK         = QT_LEFT|QT_RIGHT
;GAME_SELECT     = $00
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
;frameCntHi      .byte               ; TODO: enable demo mode after (demo?) game over, in select mode (5 minutes? = 24000 frames)
randomLo        .byte
  IF RAND16 ;{
randomHi        .byte
  ENDIF ;}
debounce        .byte               ; D.......  Debounce ; TODO: = powerTimLst?
;---------------------------------------
lineLst         ds  NUM_PLAYERS
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
xPlayerFract    .byte
enemySpeed      .byte
xEnemyFract     .byte
bonusSpeed      = enemySpeed
xBonusFract     .byte
maxLine         .byte               ; permanent value instead of calculated from scratch
firstPlayer     .byte               ; used if Get1stPlayerScore takes too long
; reused:
lastButtons     = playerSpeed       ; BBBBBBBB  reused during GAME_SELECT & GAME_OVER
waitedOver      = xPlayerFract      ; ......WW  reused during GAME_OVER
playerIdx       = enemySpeed        ; ....PPPP  reused during GAME_OVER (score displayed when game is over)
ignoredScores   = xEnemyFract       ; IIIIIIII  reused for displaying alternating scores
nxtIgnoredScores= xBonusFract
;---------------------------------------
; row bits:
  IF SWITCH_DIR
buttonBits      .byte               ; 1 = pressed
  ENDIF
playerLeft      .byte               ; 1 = left, 0 = right
enemyLeft       .byte               ; 1 = left, 0 = right
bonusLeft       .byte               ; 1 = left, 0 = right
playerDone      .byte               ; 0 = alive, 1 = dead/score rolled
enemyEyes       .byte               ; 0 = alive, 1 = eyes
;---------------------------------------
powerTimLst     ds NUM_PLAYERS              ;  8 bytes
  IF POWER_CNT ;{
powerTimCnt     .byte
  ENDIF ;}
;---------------------------------------
audIdxLst       ds  2
audIdx0         = audIdxLst
audIdx1         = audIdxLst+1
;---------------------------------------
scoreLst        ds  NUM_PLAYERS*2           ; 16 bytes
scoreLoLst      = scoreLst
scoreHiLst      = scoreLst+NUM_PLAYERS
NUM_RESETS      = . - resetLst
;---------------------------------------
HISCORE_BYTES   = 3
hiScoreLst      ds  HISCORE_BYTES   ; must be followed by gameState for PlusROM HSC
hiScoreLo       = hiScoreLst
hiScoreHi       = hiScoreLst+1
hiScoreLine     = hiScoreLst+2
gameState       .byte               ; MMrLDBVV  Mode, Right/Left QuadTari, Dinged, Bonus, Variation
;---------------------------------------
countDown       = scoreLoLst        ; reused during GAME_START
;---------------------------------------
  IF PLUSROM_LOAD
delayRequest   .byte                ; wait before loading high score
  ENDIF
cxPelletBits    .byte               ; temporary
cxSpriteBits    .byte               ; temporary

RAM_END         = .

; temp vars and stack form a consecutive area:
tmpVars         = $100 - STACK_SIZE - NUM_TMPS

  IF PLUSROM
WriteToBuffer       = $1ff0     ; MUST be $1xxx (used for detection)!
WriteSendBuffer     = $1ff1
ReceiveBuffer       = $1ff2
ReceiveBufferSize   = $1ff3
  ENDIF

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
    IF (PASS > 0) && (>{1} != >{2})
     LIST ON
      ECHO ""
     IF {3} != ""
      ECHO "ERROR: different pages! (", {3}, "=", {2}, ",", {1}, ")"
     ELSE
      ECHO "ERROR: different pages! (", {2}, ",", {1}, ")"
     ENDIF
      ECHO ""
      ERR
    ENDIF
    LIST ON
  ENDM

  MAC CHECKPAGE_LBL
    LIST OFF
      _CHECKPAGE ., {1}, {2}
  ENDM

  MAC CHECKPAGE
    LIST OFF
    CHECKPAGE_LBL {1}, ""
  ENDM

  MAC CHECKPAGE_DATA_LBL
    LIST OFF
_ADDR SET . - 1 ; hack to convince DASM
    _CHECKPAGE _ADDR, {1}, {2}
  ENDM

  MAC CHECKPAGE_DATA
    LIST OFF
    CHECKPAGE_DATA_LBL {1}, ""
  ENDM

  MAC VERSION_STR
    .byte   ((VERSION & $f00) >> 8) + 48
    .byte   "."
    .byte   ((VERSION & $0f0) >> 4) + 48
   IF VERSION < $100 | (VERSION & $00f) != 0
;   IF (VERSION & $00f) != 0
    .byte   ((VERSION & $00f) >> 0) + 48
   ENDIF
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
    LIST OFF
   IF (>(. + {1} - 1)) > (>.)
    ALIGN_FREE_LBL {2}, {3}
   ENDIF
    LIST ON
  ENDM

  MAC COND_ALIGN_FREE ; space required, alignement
    LIST OFF
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

  MAC NEXT_PASS
   IFNCONST PASS
PASS SET 0
   ELSE
PASS SET PASS + 1
    ECHO "Pass:", [PASS]d
   ENDIF
  ENDM

  NEXT_PASS

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

 IF PLUSROM
PLUSROM_ID  = 84                ;           Pac-Line Panic game id in Highscore DB

  MAC COMMIT_PLUSROM_SEND
    lda     #PLUSROM_ID
    sta     WriteSendBuffer     ;           send request to backend...
  ENDM
 ENDIF ; /PLUSROM


;===============================================================================
; R O M - C O D E
;===============================================================================
    SEG     Bank0
    ORG     BASE_ADR

COPYRIGHT_LEN SET 0

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
    stx     frameCnt        ; required for alternating from scratch in select mode

; setup initial board:
    ldx     #NUM_PLAYERS-1
.loopPf
    jsr     SetupPowerPellets
  IF BIG_MOUTH = 0
    lda     #SCW*1/2-16     ; at right of left power-ups
  ELSE
    lda     #SCW*1/2-16-1   ; at right of left power-ups
  ENDIF
    sta     xPlayerLst,x
    lda     #SCW-8-1        ; right border-1
    sta     xEnemyLst,x
    lda     #X_BONUS_OFF    ; no bonus
    sta     xBonusLst,x
    lda     #1
    sta     lineLst,x
    dex
    bpl     .loopPf

    stx     VDELP0
    stx     VDELBL
    rts
; /GameInit

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
    lda     #%010000            ; 2
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
; TODO?: use for lineLst,x >> 2, and #$07

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
  ELSE ;{THEME_ALT_1|THEME_ALT_2|THEME_ALT_3
    sta     WSYNC               ; 3 =  3
;---------------------------------------
    sty     GRP1                ; 3         Y == 0!
    SLEEP   13
  ENDIF ;}

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
    lda     xPowerLst,x         ; 4         x-pos
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
TIM_1a ; timers have problems with preceding WSYNCs!
; setup player sprite pointer:
    lda     PlayerCol,x         ; 4
    sta     .color0             ; 3
    lda     playerDone          ; 3
    and     Pot2Bit,x           ; 4
    beq     .playerAlive        ; 2/3=16/17
; death animation:
    lda     powerTimLst,x       ; 4
    lsr                         ; 2
    lsr                         ; 2
    lsr                         ; 2
    clc                         ; 2
    adc     #NUM_ALIVE_PTR      ; 2
    tay                         ; 2
    bpl     .playerLeft         ; 3 = 19+7

.playerAlive
    lda     xPlayerLst,x        ; 4         animate in sync with position (= speed based)
    and     #7                  ; 2
    tay                         ; 2
    lda     playerLeft          ; 3
    and     Pot2Bit,x           ; 4
    bne     .playerLeft         ; 2/3
    iny                         ; 2         adjust animation when going right
.playerLeft
    lda     PlayerPtr,y         ; 4
    sta     .ptr0               ; 3 = 25/26
; setup power-up pointer:
    ldy     #2                  ; 2
    lda     #0                  ; 2
TIM_1b  ; 46..47 (+6) cycles
    sta     WSYNC               ; 3 =  7
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
    bit     .bonusFlag          ; 3         bonus frame?
    bmi     .drawEnemy          ; 2/3        no, flicker draw enemy
    lda     xBonusLst,x         ; 4
    eor     #X_BONUS_OFF        ; 2
    beq     .drawEnemy          ; 2/3=13/14
; draw bonus:
    lda     lineLst,x           ; 4
  REPEAT BONUS_SHIFT
    lsr                         ; 2/4/6     every 4th line
  REPEND                        ;   =  6..10
  IF MAX_BONUS
    cmp     #NUM_BONUS          ;
    bcc     .lowerLine
    lda     #0                  ;           show top bonus
.lowerLine
  ENDIF
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

.powerOff
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
    NOP_W                       ; 2
.hideEnemy
    ldy     #<NoEnemyGfx        ; 2
    bne     .setPtr1            ; 3

.drawEnemy                      ;
    lda     Pot2Bit,x           ; 4
    bit     playerDone          ; 3
    bne     .hideEnemy          ; 2/3
    bit     enemyEyes           ; 3
    bne     .drawEyes           ; 2/3= 9/10
; setup colors:
    lda     powerTimLst,x       ; 4
    beq     .powerOff           ; 2/3= 6/7
; power on:
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
    ldy     #<EnemyDarkGfx1     ; 2 = 10
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
TIM_2b ; 101..146 (+6) cycles (76 * 3 = 158)
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

    ldx     #5                  ; 2
.waitPellet
    dex                         ; 2
    bne     .waitPellet         ; 3/2       = 26
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
    rol     cxPelletBits        ; 5         P0 vs PF
    lda     CXPPMM              ; 3         (write: COLUP1)
    asl                         ; 2
    rol     cxSpriteBits        ; 5         P0 vs P1
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
;---------------------------------------
    lda     #FRAME_COL
    sta     WSYNC
;---------------------------------------
    sta     COLUBK
    sta     WSYNC
;---------------------------------------
    sty     COLUBK
  ENDIF
    stx     VBLANK
; /DrawScreen

    ; falls through

;---------------------------------------------------------------
OverScan SUBROUTINE
;---------------------------------------------------------------
; - determine maximum human player line
; - handle player/pellet collisions
; - handle player/enemy collisions
; - update audio

.loopCnt        = tmpVars
.playerAI       = tmpVars+1
.xPos           = tmpVars+2

  IF NTSC_TIM
    lda     #37+1                   ; allows up to 2368 cycles
  ELSE
    lda     #63
  ENDIF
    sta     TIM64T
TIM_OS                              ; ~2293 cycles (maxLine  permanent, mainly human players, less with 50% check)

    bit     gameState
    bmi     .startRunningMode       ; GAME_RUNNING|GAME_OVER
.notRunning                         ; GAME_SELECT|GAME_START|GAME_OVER
    jmp     .skipGameRunning

;---------------------------------------
.skipCXPelletJmp
    jmp     .skipCXPellet
;---------------------------------------

.startRunningMode
    bvc     .notRunning             ; GAME_OVER
    ldx     #NUM_PLAYERS-1
.loopPlayers
    stx     .loopCnt
; extract AI row info for easier, more compact further processing:
    lda     playerAI
    cmp     #$ff
    beq     .setPlayerAI            ; demo mode?
    and     Pot2Bit,x
    beq     .setPlayerAI
    lda     #1
.setPlayerAI
    sta     .playerAI               ; = 0, 1, $ff
; 15 bytes
;---------------------------------------
; *** handle player/pellet collisions ***
    asl     cxPelletBits
    bcc     .skipCXPelletJmp
; only check 50% of the players each frame:
    txa
    eor     frameCnt
    lsr
    bcc     .skipCXPelletJmp
; check player already dead:
    lda     playerDone
    and     Pot2Bit,x
    bne     .skipCXPelletJmp
; eat pellet:
  IF 0 ;{
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
    bne     .skipCXPelletJmpX
  ENDIF ;}
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
    cmp     pfLst,x             ; value changed? (check required!)
    sta     pfLst,x
    ldx     .loopCnt            ; restore X
    bcs     .skipCXPelletJmp    ;  nope
    sty     .xPos
; check if power-up got eaten:
    lda     xPowerLst,x
;    sec
    sbc     #6-1                ; CF == 0
    lsr
    lsr
    lsr
    eor     .xPos
    bne     .noPower
    sta     xPowerLst,x
  IF POWER_CNT ;{
    lda     powerTimCnt
    ora     Pot2Bit,x
    sta     powerTimCnt
   ENDIF ;}
    lda     #POWER_TIM
    sta     powerTimLst,x
    lda     #POWER_PTS
    NOP_W
.noPower
    lda     #PELLET_PTS         ; = 1
    jsr     AddScoreLo
    lda     .playerAI
  IF DEMO_SOUND
    bmi     .demoWakaSound
  ENDIF
    bne     .skipWaka           ; only human players make sound
.demoWakaSound
; Waka-Waka alternates between WAKA und KAWA sounds:
    lda     audIdx0
    cmp     #BONUS_END-1        ; other sound playing?
    bcc     .updataWaka         ;  no, updata waka
  IF DEMO_SOUND
    cmp     #DEATH_END          ; death sound? (ranges behind sound data and might be 0)
    bcs     .skipWaka           ;  yes, skip waka
  ENDIF
    tay
    lda     AudF0Tbl,y          ; other sound at end?
    bne     .skipWaka           ;  yes, skip waka, else start with 0
.updataWaka
    cmp     #WAKA_START         ; currently kawa played?
    bcs     .playsKawa          ;  yes, switch from kawa
    cmp     #WAKA_MID
    bcs     .skipWaka
    adc     #WAKA_LEN * 3       ; switch from waKA to KAwa
    bne     .setWakaIdx

.playsKawa
    cmp     #KAWA_MID
    bcs     .skipWaka
    sbc     #WAKA_LEN - 1       ; switch from kaWA to WAka
.setWakaIdx
    sta     audIdx0
.skipWaka
; check if all pellets cleared:
    lda     pf01LeftLst,x
    ora     pf20MiddleLst,x
    ora     pf12RightLst,x
    bne     .skipCXPellet
; increase speeds:
; increase speed only for 1st player reaching next line:
    lda     lineLst,x
    cmp     maxLine
    bcc     .skipIncSpeed
    lda     .playerAI           ; only human players increase speeds (except for demo mode)
    bmi     .demoModeInc        ; demo mode
    bne     .skipIncSpeed
.demoModeInc
    inc     maxLine
    lda     playerSpeed
;    clc
    adc     #DIFF_PL_SPEED-1    ; CF == 1
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
    bcs     .skipIncSpeed
    sta     enemySpeed
;    sta     bonusSpeed         ; = enemySpeed
.skipIncSpeed
  IF LIMIT_LINE ;{
    ldy     lineLst,x
    iny
    cpy     #100
    bcc     .incLine
    ldy     #100 - (1<<BONUS_SHIFT)*NUM_BONUS ; reset line to e.g. 68
.incLine
    sty     lineLst,x
  ELSE ;}
    inc     lineLst,x
  ENDIF
; new line with random power-up & pellets:
    jsr     SetupPowerPellets
; reset bonus:
    ldy     #X_BONUS_OFF        ; disable bonus
    lda     gameState
    and     #NO_BONUS_GAME
    bne     .skipBonus
    lda     lineLst,x
    and     #BONUS_MASK         ; every 4th line
    bne     .skipBonus
    txa
    eor     randomLo
;    jsr     NextRandom
    and     Pot2Bit,x
    eor     bonusLeft
    sta     bonusLeft
    ldy     #SCW/2-4            ; start bonus at center
.skipBonus
    sty     xBonusLst,x
.skipCXPellet
;---------------------------------------
; *** handle player/enemy/bonus collisions ***
    asl     cxSpriteBits
    lda     Pot2Bit,x
    and     playerDone
    bne     .skipCXSpriteJmp
; player alive, check:
    bcc     .skipCXSpriteJmp
; alternate between bonus and enemy frames
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
    lda     .playerAI
  IF DEMO_SOUND
    bmi     .demoBonusSound
  ENDIF
    bne     .skipBonusSound
.demoBonusSound
    lda     audIdx0
    cmp     #DEATH_END
    bcs     .skipBonusSound
    lda     #BONUS_START
    sta     audIdx0
    lda     #BONUS_VOL
    sta     AUDV0
.skipBonusSound
; add bonus points:
    lda     lineLst,x
    sbc     #1                  ; or 2, does not matter here
  REPEAT BONUS_SHIFT
    lsr
  REPEND
  IF MAX_BONUS
    cmp     #NUM_BONUS
    bcc     .lowerLine
    lda     #NUM_BONUS-1        ; score top bonus
.lowerLine
  ENDIF
    and     #NUM_BONUS-1
    tax
    lda     BonusScore,x
    ldy     BonusScoreHi,x
    ldx     .loopCnt
  IF MULT_SCORE
    clc
    sed
  ENDIF
    jsr     AddScore
.skipCXSpriteJmp
    jmp     .skipCXSprite

.checkEnemy
    lda     Pot2Bit,x           ; already dead?
    bit     enemyEyes
    bne     .skipCXSpriteJmp    ;  yes, skip

; must overlap at least 4 pixels:
    lda     xPlayerLst,x
    sec
    sbc     xEnemyLst,x
    adc     #4
    cmp     #4*2
    bcs     .skipCXSpriteJmp

    lda     Pot2Bit,x
    ldy     powerTimLst,x       ; power-up enabled?
    bne     .killEnemy          ;  yes, kill enemy
; kill player:
    lda     .playerAI
    bmi     .doDeathSound       ; demo mode, enable counter in audIdx0 in demo mode (for complete death animation of last AI player)
    bne     .skipKillSound
; player death sound and animation:
.doDeathSound
    lda     #DEATH_START
    sta     audIdx0
.skipKillSound
    lda     #(NUM_DEATH_PTR << 3) - 2   ; start death animation (1 shorter to always finish death animation)
    sta     powerTimLst,x
; disable player:
    lda     Pot2Bit,x
    ora     playerDone
    sta     playerDone
    bne     .skipCXSpriteJmp

.killEnemy
    ora     enemyEyes           ; -> eyes
    sta     enemyEyes
    lda     .playerAI
  IF DEMO_SOUND
    bmi     .demoEatenSound
  ENDIF
    bne     .skipEatenSound
.demoEatenSound
    lda     #EATEN_START
    sta     audIdx1
    lda     #$c
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
    lda     #0                  ; disable power-up
    sta     powerTimLst,x
  IF POWER_CNT ;{
    lda     powerTimCnt
    and     Pot2Mask,x
    sta     powerTimCnt
  ENDIF ;}
.skipCXSprite
;---------------------------------------
; loop:
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
  IF HISCORE_DING
    cpy     #HISCORE_END
    bcc     .notHiScore
    tya
    sbc     #HISCORE_END+DING_LEN
    bcc     .longDing
    ldx     #NUM_SHORT_DINGS-1  ; number of short repeats
.loopHiScore
    dex
    sbc     #SHORT_DING_LEN     ; repeated sequence length
    bcs     .loopHiScore
.longDing
    tax
    lda     DingVolTbl-$100+DING_LEN,x
    sta     AUDV1
    lda     #$4
    sta     AUDC1
    ldx     #$04                ; or 5
    bne     .setAudF1

.notHiScore
  ENDIF
    cpy     #EYES_END           ; eaten or eyes sound playing?
    bcs     .contEnemySound     ;  yes, continue current sound
; enemy eyes?
    lax     playerAI
  IF DEMO_SOUND
    inx                         ; demo mode?
    bne     .notDemoEyes        ;  no, ignore AI and dead players
    txa                         ;  yes, ignore only dead players
.notDemoEyes
  ENDIF
    ldx     #EYES_IDX
    ora     playerDone
    eor     #$ff
    and     enemyEyes
    bne     .startEnemySound
; enemy scared?
    cpy     #SCARED_END         ; scared sound playing?
    bcs     .contEnemySound     ;  yes, continue current sound
 IF !POWER_CNT
    ldx     #NUM_PLAYERS-1
.loopPowerTim
    lda     powerTimLst,x
    beq     .nextPlayer
    lda     playerAI
  IF DEMO_SOUND
    cmp     #$ff                ; demo mode?
    bne     .notDemoScared      ;  no, ignore AI and dead players
    lda     #0                  ;  yes, ignore only dead players
.notDemoScared
  ENDIF
    ora     playerDone          ;  no scared audio if powerTimLst,x is currently used for death animation
    and     Pot2Bit,x
    bne     .nextPlayer
    ldx     #SCARED_IDX
    bne     .startEnemySound

.nextPlayer
    dex
    bpl     .loopPowerTim
; up to 233 cycles
 ELSE ;{ POWER_CNT
    lax     playerAI
    eor     #$ff                ; demo mode?
   IF DEMO_SOUND
    bne     .contScaredSound    ;  yes, ignore only dead players
    txa                         ;  no, ignore AI and dead players
.contScaredSound
   ENDIF
    ldx     #SCARED_IDX
    and     powerTimCnt
    bne     .startEnemySound
 ENDIF ;}

; siren sound?
  IF !DEMO_SOUND
    lda     .playerAI           ; ignore AI lines
    bmi     .stopSound1         ;  demo mode, no sound
  ENDIF
    cpy     #SIREN_END          ; siren sound playing?
    bcs     .contEnemySound     ;  yes, continue current sound
    ldx     #SIREN_IDX
.startEnemySound
    lda     #$4                 ; AudC1Tbl,x
    sta     AUDC1
    lda     AudV1Tbl,x
    sta     AUDV1
    ldy     AudStart1Tbl,x
.contEnemySound
    ldx     AudF1Tbl,y
    beq     .stopSound1
    cpy     #SIREN_START+1
    bcs     .setAudF1
    lda     maxLine             ; increase pitch every 2nd line
    lsr
    eor     #$ff
    cmp     #-12-1              ; decrease AUDF1 by up to 10
    bcs     .addLine
    lda     #-12
.addLine
    adc     AudF1Tbl,y
    tax
.setAudF1
    stx     AUDF1
    dey
    NOP_W
.stopSound1
    ldy     #0
    sty     audIdx1
.skipGameRunning
; 2133 cyles for loop
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

    lda     #%10
    sta     WSYNC
;---------------------------------------
    sta     VSYNC

; *** Player sounds ***
TIM_A0S ; 40 (all AI)..102 (all human)
    ldy     audIdx0
    cpy     #DEATH_END
    bcc     .notDeath
; 1st or 2nd part?
    tya
    sbc     #DEATH_END1
    bcs     .deathPart1
    ldx     #0
    and     #$07                ; == DEATH_LEN2 - 1
    bpl     .contDeathPart2

.deathPart1
; TODO? count up from zero? and subtract AudF0Tbl?
    ldx     #6                  ; number repeats * 2
.loopDeath
    dex
    sbc     #DEATH_LEN1         ; repeated sequence length
    bcs     .loopDeath
; A = -DEATH_LEN1 .. -1
    adc     #DEATH_LEN1 + DEATH_LEN2
.contDeathPart2
    sta     WSYNC
;---------------------------------------
    tay
    txa
  IF !DEMO_SOUND
    ldx     .playerAI
    bmi     .contDeath          ; X == $ff! -> mute sound
  ENDIF
    asl
    adc     AudF0Tbl+DEATH_END,y
    tax
    lda     #DEATH_VOL
    sta     AUDV0
    lda     #$4
    bne     .contDeath

.notDeath
    sta     WSYNC
;---------------------------------------
    ldx     AudF0Tbl,y          ; varies
    beq     .stopSound0
    lda     #$c
.contDeath
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
TIM_A0E
    sta     WSYNC
;---------------------------------------
    dec     frameCnt

  IF NTSC_TIM
    ldy     #40-1               ; allows up to 2432 cycles
  ELSE
    ldy     #77
  ENDIF
    ldx     #$80
    lda     SWCHB
    and     #%11
    cmp     #%11
    sta     WSYNC
;---------------------------------------
    stx     VSYNC
    sty     TIM64T
TIM_VS                          ; ~2383 cycles (all AI players) 2383

; *** check console switches ***
    bcs     .notSwitched
    lsr                         ; put RESET bit into carry
    lda     debounce            ; DEBOUNCE?
    bmi     .skipSwitches
;    ora     #DEBOUNCE
;    sta     debounce
    stx     debounce            ; = $80
ButtonReset
    php                         ; remember RESET bit
    jsr     GameInit
    lax     gameState
    and     #~GAME_MASK
;    ora     #GAME_SELECT       ; = $00
    cmp     gameState
    sta     gameState
    beq     .alreadyInSelect
    stx     hiScoreVar          ; remember game variation of current high score
    plp
    bcs     .skipSwitches       ; ignore SELECT once
    NOP_B
.alreadyInSelect
    plp                         ; restore RESET bit
  IF !PLUSROM_LOAD
    bcc     .contReset
  ELSE
    bcs     .contSelect
    jmp     .contReset

.contSelect
  ENDIF
; handle SELECT switch:
  IF NUM_VARIATIONS = 8 | NUM_VARIATIONS = 4
    adc     #1-1
    and     #VAR_MASK           ; increase game variation
  ELSE ;{
    and     #VAR_MASK           ; increase game variation
    tay
    iny
    cpy     #NUM_VARIATIONS
    bcc     .skipResetVar
    ldy     #0
.skipResetVar
    tya
  ENDIF ;}
    eor     gameState
    and     #VAR_MASK
    eor     gameState
    sta     gameState
  IF PLUSROM_LOAD
    lda     #60                 ; initiate request
    sta     delayRequest
  ENDIF
    NOP_W
.notSwitched
;    lda     debounce
;    and     #~DEBOUNCE
;    sta     debounce
    asl     debounce            ; $80 -> $00
.skipSwitches
  IF PLUSROM_LOAD
    lda     delayRequest
    beq     .skipRequest
    dec     delayRequest
    bne     .skipRequest
DEBUG2
; start request:
    lda     gameState
    and     #VAR_MASK
    sta     hiScoreVar
    sta     WriteToBuffer       ; Y = game variation as stored in DB
    lda     #PLUSROM_ID         ; Pac-Line Panic HSC game-id (84 = %01010100)
    sta     WriteSendBuffer     ; send request
.skipRequest
; check if response is complete:
    ldx     #HISCORE_BYTES
    cpx     ReceiveBufferSize
    bne     .skipReadResponse
DEBUG1
; read response:
.loopReadHiScore
    lda     ReceiveBuffer       ; line, hi, lo
    sta     hiScoreLst-1,x
    dex
    bne     .loopReadHiScore
; minimum delay before any pending request:
    lda     delayRequest
    beq     .skipReadResponse
    lda     #8
    sta     delayRequest
.skipReadResponse
  ENDIF
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
    sta     WSYNC               ; wait at least one scanline
;---------------------------------------
    and     SWCHA
    ora     .tmpButtons         ; %76435410
    eor     #$ff
    tay
    eor     lastButtons         ; last buttons ^ new buttons
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
    eor     #GAME_OVER          ;  ^GAME_SELECT (=$00)
    sta     hiScoreVar
    clc
ContInitCart                    ; enters with CF=1
    eor     #GAME_OVER
    sta     gameState
    jmp     ButtonReset

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
    sta     hiScoreLine
.skipResetHiScore

;---------------------------------------------------------------
; wait for button release & press to switch to start
.contReset
    lda     #GAME_START         ; ^GAME_SELECT = $00
    eor     gameState
    sta     gameState

    lda     #COUNT_START
    sta     countDown
    lda     #30
    sta     frameCnt

.tmpXPowerLst = scoreHiLst      ; reused during GAME_START
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
    stx     playerAI            ; = $ff
    bne     .skipRunningJmp     ; skip checking both QuadTaris this frame again

; falls  through

;---------------------------------------------------------------

.startMode
    ldx     #NUM_PLAYERS-1
.loopStart
; coloring:
; 1. not pressed, not human: dark sprites, no pellets
; 2. not pressed, human: bright sprites, no pellets
; 3. pressed, human: bright sprites, show pellets

; if human, show pellets and power-up while no button is pressed:
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
    bne     .checkButton        ; left/right bits are set opposite!
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
  IF EXTEND_COUNTDOWN
    cmp     playerAI
    beq     .setPlayerAI
    ldy     countDown
    cpy     #COUNT_START
    bcs     .setPlayerAI
    inc     countDown
.setPlayerAI
    inc     frameCnt
  ENDIF
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
    sta     WSYNC               ; wait at least one scanline
;---------------------------------------
.skip2ndSetA
    dex
    bpl     .loopStart

    lda     frameCnt
    bpl     .skipRunningJmp2
    dec     countDown
    beq     .switch2Running
    lda     #WAKA_START
    sta     audIdx0
  IF !EXTEND_COUNTDOWN
    lda     #FPS
  ELSE
    lda     #FPS*3/4
  ENDIF
    sta     frameCnt
    bne     .skipRunningJmp2

.switch2Running
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
; switch to running mode & initialize speeds:
    lda     gameState
    eor     #GAME_START^GAME_RUNNING
  IF HISCORE_DING
    and     #~HISCORE_DINGED
  ENDIF
    sta     gameState
    and     #DIFF_MASK
    tay
    lda     PlayerSpeeds,y
    sta     playerSpeed
    lda     EnemySpeeds,y
    sta     enemySpeed
;    sta     bonusSpeed
    lda     VarLines,y
    ldx     #NUM_PLAYERS-1
.loopLines
    sta     lineLst,x
    dex
    bpl     .loopLines
    sta     maxLine
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
TIM_SPS ; 57..87 cycles (move to VSYNC?)
    lda     #0                  ; 2
    sta     .playerSpeed        ; 3
    sta     .enemySpeed         ; 3
    sta     .bonusSpeed         ; 3

    lda     playerSpeed
    asl
    rol     .playerSpeed
    adc     xPlayerFract
    sta     xPlayerFract
    bcc     .skipIncPlayer
    inc     .playerSpeed        ;           -> 0..2
.skipIncPlayer
    lda     enemySpeed
    asl
    rol     .enemySpeed
    adc     xEnemyFract
    sta     xEnemyFract
    bcc     .skipIncEnemy
    inc     .enemySpeed         ;           -> 0..2
.skipIncEnemy
    lda     frameCnt            ; 3
    lsr                         ; 2         50% of enemy speed
    bcc     .skipBonusSpeed     ; 2/3=  7/8
    lda     bonusSpeed          ; 3         = enemySpeed
    asl                         ; 2
    rol     .bonusSpeed         ; 5
    adc     xBonusFract         ; 3
    sta     xBonusFract         ; 3
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
; check for game over:
; (let game continue until end of death sound)
    lda     powerTimLst,x       ; player dead?
    beq     .skipAnim           ;  yes, skip further animation
    dec     powerTimLst,x
.skipAnim
    lda     audIdx0
    cmp     #DEATH_END          ; other (human) player still dying?
    bcc     .doneDying          ;  nope, check if all dead
.nextMoveJmp
    jmp     .nextMove

.doneDying
    lda     playerDone
    ldy     playerAI
    iny                         ; demo mode?
    beq     .wait4AllDone       ;  yes
    ora     playerAI
.wait4AllDone
    eor     #$ff
    bne     .nextMoveJmp
; game over, display scores:
    sta     lastButtons         ; A = 0
    sta     AUDV1
    sta     frameCnt            ; make score display loop start from 1.
    sta     playerIdx
    sta     ignoredScores
    lda     #2
    sta     waitedOver
; check for new high score:
    tya                         ; demo mode? (Y = playerAI + 1)
    beq     .skipHiScore        ;  yes
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
    lda     lineLst,x
    sta     hiScoreLine
  IF PLUSROM
; only new high scores are send:
    ldx     #HISCORE_BYTES      ; including gameState
.loopSendHiScore
    lda     hiScoreLst,x
    sta     WriteToBuffer       ; var, line, hi, lo
    dex
    bpl     .loopSendHiScore
    COMMIT_PLUSROM_SEND
  ELSE
    ds      34-16, $ea
  ENDIF
.skipHiScore
    lda     gameState
    eor     #GAME_RUNNING^GAME_OVER
    sta     gameState
    jmp     .skipRunning

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
    cmp     #SCW-SPRITE_W   ; 152 , player at very left?
    bcc     .normalPos      ;  nope
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

; *** Move bonus ***
    lda     frameCnt
    lsr
    bcc     .skipBonus
    ldy     xBonusLst,x
    cpy     #X_BONUS_OFF        ; disabled bonus?
    beq     .skipBonus          ;  yes, do not move
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
;    beq     .skipTim            ; superfluous check
    sec
    sbc     .tmpSpeed           ; decrease in sync with speed
    bcs     .timOk
  IF POWER_CNT ;{
    lda     powerTimCnt
    and     Pot2Mask,x
    sta     powerTimCnt
  ENDIF ;}
    lda     #0
.timOk
    sta     powerTimLst,x
.skipTim

;---------------------------------------------------------------
; *** update player directions ***
; check buttons:
    lda     SWCHA
    and     ButtonBit,x
    cmp     #1
    lda     playerAI
    tay
    and     Pot2Bit,x
    bne     .aiPlayer
  IF SWITCH_DIR & BUTTON_DIR
    bit     SWCHB
    bvc     .buttonDir
  ENDIF
  IF SWITCH_DIR ;
    lda     buttonBits
    and     Pot2Mask,x          ; clear button bit
    bcs     .setButtonBits      ; not pressed, set cleared bit
    cmp     buttonBits          ; bit set before?
    bne     .skipReverseDir     ;  yes, skip reversing
    lda     Pot2Bit,x
    eor     playerLeft
    sta     playerLeft
    lda     buttonBits
    ora     Pot2Bit,x           ; set button bit
.setButtonBits
    sta     buttonBits
.skipReverseDir
    jmp     .skipAI
  ENDIF
  IF BUTTON_DIR
.buttonDir
    bcs     .moveRight
    bcc     .moveLeft
  ENDIF

.aiPlayer
    bcs     .contAI
; button of AI player pressed:
    iny                         ; demo mode?
    bne     .contAI             ;  no, continue with AI
    jmp     ButtonReset         ;  yes, reset game

.contAI

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
; *** AI Logic ***
; 1. chase ghost
;    - while power is enabled
;    - stop chasing when power time runs out
; 2. run away from ghost without power if ghost is close
;    - close means, distance to ghost smaller than 1.5x distance to opposite border
    lda     maxLine
    cmp     #24
    ldy     powerTimLst,x
    bcs     .highLines
    cmp     #16
    bcs     .midLines
    cpy     #POWER_TIM*1/8
    NOP_W
.midLines
    cpy     #POWER_TIM*1/6
    NOP_W
.highLines
    cpy     #POWER_TIM*1/4
    lda     xPlayerLst,x
    bcc     .noAIPower
 ; AI player has power:
    cmp     xEnemyLst,x
    bcs     .moveLeft
    bcc     .moveRight

.noAIPower
; AI player has no power:
    cmp     xEnemyLst,x
    bcc     .enemyRight
;.enemyLeft
; enemy left:
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
    sta     WSYNC           ; 3         TODO: WHY is this required for Stella???
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
;   - 1st player's score and line (alternating)
; - GAME_OVER:
;   - nth player's score and line (alternating)
;   - high score
; - GAME_SELECT
;   - start line and bonus
;   - high score (if saved)
; - GAME_START
;   - count down

; ******************** Setup Score ********************
.maxLo          = tmpVars
.tmpIgnored     = tmpVars+1
.countHuman     = tmpVars+2
.char0          = tmpVars+3
.char1          = tmpVars+4
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
    sty     .char1
    ldy     #NUM_PLAYERS-1

    bit     gameState
    bpl     .notRunningMode
    jmp     .startRunningMode       ; GAME_START|GAME_RUNNING

.notRunningMode
    lda     #SCORE_TIM*2
    cmp     frameCnt
    bcs     .contFrameCnt
    sta     frameCnt
.contFrameCnt
    bvc     .selectMode             ; GAME_SELECT
;---------------------------------------------------------------
; GAME_OVER

; if all players are AI (demo mode), show all 8 AI scores:
    lax     playerAI
    inx                             ; demo mode?
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
    cmp     #SCORE_TIM*2
    bne     .sameRank
    dex
    bpl     .skipReset
    lsr     waitedOver          ; one whole score loop done
    lda     playerAI
    tay
    iny                         ; demo mode?
    bne     .notDemoMode        ;  no, show only human scores
    bcc     .notLooped          ; first loop, continue
    clc                         ; full score loop in demo mode
    jmp     ButtonReset         ;  reset

.notLooped
    tya                         ; demo mode, show all scores (Y = 0)
.notDemoMode
    sta     nxtIgnoredScores
    ldx     .countHuman
    lda     hiScoreLine
    beq     .hiScoreNotSet
    inx                         ; display high score first
.hiScoreNotSet
.skipReset
    lda     nxtIgnoredScores
    sta     ignoredScores
    stx     playerIdx
.sameRank
; check for high score display:
    cpx     .countHuman
    beq     .notHighScore
    bcs     .highScore
.notHighScore
; select n-largest score to display:
    lda     ignoredScores
    jsr     GetNthPlayerScore
    sta     nxtIgnoredScores
; display rank & score | player id & line
    lda     .countHuman
    sec
    sbc     playerIdx
    sta     .char0              ; displayed rank - 1
    inc     .char0
    lda     #SCORE_TIM
    cmp     frameCnt
    bcs     .displayRankLine
    ldy     #ID_DOT
    sty     .char1
    bcc     .contGameOver

.highScore
    lda     #SCORE_TIM
    cmp     frameCnt
.displayHighScore
    ldx     #NUM_PLAYERS
    stx     firstPlayer
    lda     #ID_LETTER_H
    sta     .char0
    lda     #ID_LETTER_I
    sta     .char1
    lda     hiScoreLo
    ldy     hiScoreHi
    bcc     .contDisplayHighScore
    lda     hiScoreLine
    bcs     .contDisplayHighLine
;---------------------------------------------------------------
.selectMode
; display bonus mode and starting line (B.LnLL), alternating with high score :
    iny
    sty     firstPlayer         ; Y = NUM_PLAYERS -> white display
    lda     hiScoreLine         ; any highscore existing?
    beq     .skipShowHiScore    ;  no, do not show high score
    lda     gameState
    eor     hiScoreVar
    and     #VAR_MASK           ; high score for current game variation?
    bne     .skipShowHiScore    ;  no, do not show high score
    lda     frameCnt
    cmp     #SCORE_TIM          ; 2nd half of display cycle?
    bcc     .displayHighScore   ;  yes, show high score
.skipShowHiScore
    ldy     #ID_BLANK
    lda     gameState
    and     #NO_BONUS_GAME
    bne     .noBonus
    ldy     #ID_BONUS
.noBonus
    sty     .char0
    lda     gameState
    and     #DIFF_MASK          ; game variation diff
    tay
    lda     VarLines,y
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
    bne     .contStartMode
;---------------------------------------------------------------
.runningMode
TIM_SCS ; 523..759 cycles
; determine player with maximum score:
    ldx     firstPlayer
    lda     INTIM
    cmp     #(743+5+63)/64+1    ;       up to 743+5 cycles until next INTIM
    bcc     .skipGet1st
    jsr     Get1stPlayerScore   ;388    get player id of largest score
.skipGet1st
    lda     #$55                ;       2/3 score, 1/3 line
    cmp     frameCnt
.displayRankLine
    txa
    eor     #$07
    sta     .char0              ;       displayed player number - 1
    inc     .char0
.contGameOver
    lda     scoreLoLst,x
    ldy     scoreHiLst,x
.contDisplayHighScore
    sta     .scoreLo
    sty     .scoreMid
    bcc     .displayScore
; display line:
    lda     lineLst,x
.contDisplayHighLine
.contSelectMode
    HEX2BCD
    cmp     #$10
    bcs     .largeLine
    ora     #ID_BLANK<<4
.largeLine
    sta     .scoreLo
    ldx     firstPlayer
    lda     #ID_LETTER_L<<4|ID_Letter_N
    sta     .scoreMid
.displayScore
    lda     PlayerCol,x
    sta     .scoreCol
    lda     .char0          ; display bonus, player number or rank (game over)
    asl
    asl
    asl
    asl
    ora     .char1
.contStartMode
    sta     .scoreHi
; setup score into score pointers:
    ldx     #(6-1)*2
.loopScores
    cpx     #(4-1)*2        ; start of points?
    bne     .skipV          ;  no, skip
;    bit     Vectors+1       ; set V-flag to remove leading zeroes, bit 6 must be set!
    bit     $ff             ; set to $fx by .scorePtrLst+1+2,x in 1st loop
.skipV
    txa
    lsr
    lsr
    tay
    lda     .scoreLst,y
.tmpScore   = .scorePtrLst+3
;    pha                     ; overwrites .scorePtr5+1
    sta     .tmpScore
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
;    pla
    lda     .tmpScore
    and     #$0f
    bne     .skipLoZero
    bvc     .skipLoZero
;    txa                        ;           display in case of score == 0
;    beq     .skipLoZero        ;            but who cares!? :-)
    lda     #ID_BLANK
    NOP_IMM
.skipLoZero
    clv
    cmp     #NUM_LO_DIGITS
    bcc     .lowId
    adc     #NUM_HI_DIGITS-1
.lowId
    tay
    lda     DigitPtr,y
    sta     .scorePtrLst,x
    lda     #>DigitGfx          ; 2
    sta     .scorePtrLst+1,x    ; 4
    sta     .scorePtrLst+1+2,x  ; 4
    dex
    dex
    bpl     .loopScores

TIM_SCE
; /VerticalBlank
TIM_VE
    jmp     DrawScreen

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
  IF PLUSROM_LOAD
    lda     #60             ; initiate request
    sta     delayRequest
  ENDIF
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
    and     #QT_MASK        ; 2         TODO: required?
    sec
    jmp     ContInitCart

;---------------------------------------------------------------
AddScoreLo SUBROUTINE
;---------------------------------------------------------------
; A = scoreLo
.playerAI   = tmpVars + 1   ; only called from OverScan
  IF MULT_SCORE
.scoreLo    = tmpVars + 2
    sta     .scoreLo
    lda     lineLst,x
    lsr
    lsr                     ; div 4
    tay
    clc
    lda     #0
    sed
.loopMult
    adc     .scoreLo
    dey
    bpl     .loopMult
    iny
AddScore    ; bonus only
  ELSE ;{
AddScore    ; bonus only
    ldy     #0
    sed
    clc
  ENDIF ;}
    adc     scoreLoLst,x
    sta     scoreLoLst,x
    tya
    adc     scoreHiLst,x
    bcc     .setScore
    lda     Pot2Bit,x       ; stop player
    ora     playerDone
    sta     playerDone
    lda     #$99            ; limit score to 9,999
    sta     scoreLoLst,x
.setScore
    sta     scoreHiLst,x
    cld
  IF HISCORE_DING
; compare with current highscore and make sound if reached:
   IF 0 ;{
; IDEA: (X ^ frame) & %11 (two per frame)
    tay
    txa
    eor     frameCnt
    and     #%11
    bne     .exit
DEBUG1
    cpy     hiScoreHi
   ELSE ;}
    cmp     hiScoreHi
   ENDIF
    bcc     .exit
    bne     .newHiScore
    lda     hiScoreLo
    cmp     scoreLoLst,x
    bcs     .exit
.newHiScore
    lda     hiScoreLine     ; old high score existing?
    beq     .exit           ;  no, ignore
;    lda     playerAI
;    and     Pot2Bit,x
    lda     .playerAI
    bne     .exit

    lda     #HISCORE_DINGED
    bit     gameState
    bne     .exit
    ora     gameState
    sta     gameState
    lda     #HISCORE_START
    sta     audIdx1
.exit
  ENDIF
    rts

CopyRight
  IF COPYRIGHT
;    .byte   " "
    .byte   GAME_NAME
    .byte    " - V"
    VERSION_STR
   IF NTSC_COL
    .byte   " (NTSC)"
   ELSE
    .byte   " (PAL60)"
   ENDIF
    .byte   " - (C)2025 Thomas Jentzsch"
;    .byte   " "
  ELSE
   IF COPYRIGHT_SHORT
    .byte   "V"
    VERSION_STR
   IF NTSC_COL
    .byte   " NTSC "
   ELSE
    .byte   " PAL60 "
   ENDIF
   .byte   "(C)Jentzsch"
   ENDIF
  ENDIF
COPYRIGHT_LEN SET . - CopyRight

;  IF PLUSROM_LOAD
;    ECHO "*** 10 extra bytes wasted here! REMOVE! ***"
;    ds 10, 0
;  ENDIF


;===============================================================================
; R O M - T A B L E S (Bank 0)
;===============================================================================

PfOffset
    ds      2, pf01LeftLst   - pfLst        ; = 0   TODO? overlap, also reorder pf12RightLst last -> 8x 0
    ds      4, pf01LeftLst   - pfLst        ; = 0   TODO? overlap
    ds      4, pf20MiddleLst - pfLst        ; = 4
    ds      2, pf20MiddleLst - pfLst        ; = 4
    ds      4, pf12RightLst  - pfLst        ; = 8
    ds      4, pf12RightLst  - pfLst        ; = 8

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
    CHECKPAGE ScoreLums

    COND_ALIGN_FREE_LBL DIGIT_GFX_LEN, 256, "DigitGfx"

;    include     Font_Levelup.inc
    include     Font_PacMan.inc

Pot2Mask; 5x
    .byte   ~$01, ~$02, ~$04, ~$08, ~$10, ~$20, ~$40, ~$80
;---------------------------------------------------------------
SetupPowerPellets SUBROUTINE
;---------------------------------------------------------------
; generate next random number;
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
; position new power-up:
    and     #$78
    cmp     #64
    bcc     .left
    adc     #32-1       ; skip 4 middle positions
.left
; @pellet 0..7, 12..19
;    clc
    adc     #6
    sta     xPowerLst,x
SetupPellets
    lda     #$f5
    sta     pf01LeftLst,x
    lda     #$fa
    sta     pf20MiddleLst,x
    lda     #$ff
    sta     pf12RightLst,x
    rts

  IF THEME_ORG
    include     "gfx_org.h"
  ENDIF
  IF THEME_ALT_1
    include     "gfx_alt.h"
  ENDIF

;---------------------------------------------------------------
Get1stPlayerScore SUBROUTINE
;---------------------------------------------------------------
; A = n-1
TIM_GNPDS   ; 376 cycles

.maxLo      = tmpVars
.tmpIgnored = tmpVars+1

; display AI scores in demo mode:
    lda     playerAI
    cmp     #$ff            ;           demo mode?
    beq     .demoMode
    NOP_W
.demoMode
    lda     #0
GetNthPlayerScore           ;           A = previous players with higher scores
    sta     .tmpIgnored     ; 3         ignore AI players
    lda     #0              ; 2
    sta     .maxLo          ; 3
    sta     firstPlayer     ; 3         in case there are no active players
    ldx     #NUM_PLAYERS-1  ; 2 = 16
.loopNthMax
    asl     .tmpIgnored     ; 5         already selected?
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
    stx     firstPlayer     ; 3         nth largest score player idx
.nextNthMax
    dex                     ; 2
    bpl     .loopNthMax     ; 3/2       12..38
    ldx     firstPlayer     ; 3
    lda     ignoredScores   ; 3
    ora     Pot2Bit,x       ; 4
; X = nth player, A = ignored
TIM_GNPDE
    rts
; /Get1stPlayerScore

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

; split tables because IDs are limited to 0..15
DigitPtr
    .byte   <Zero, <One, <Two, <Three, <Four
    .byte   <Five, <Six, <Seven, <Eight, <Nine
ID_BLANK = . - DigitPtr
    .byte   <Blank
ID_PELLET = . - DigitPtr
    .byte   <PelletChar
NUM_LO_DIGITS = ID_PELLET + 1
ID_POWER = . - DigitPtr
    .byte   <PowerChar
ID_LETTER_L = . - DigitPtr
    .byte   <Letter_L
ID_BONUS = . - DigitPtr
    .byte   <BonusGfx
ID_LETTER_H = . - DigitPtr
    .byte   <Letter_H
NUM_HI_DIGITS  = ID_LETTER_H + 1 - NUM_LO_DIGITS
ID_DOT = . - DigitPtr - NUM_HI_DIGITS
    .byte   <DotChar
ID_Letter_N = . - DigitPtr - NUM_HI_DIGITS
    .byte   <Letter_N
ID_LETTER_I = . - DigitPtr - NUM_HI_DIGITS
    .byte   <Letter_I

HMoveTbl
; this is calculated with 1 cycle extra on access
; it MUST NOT be at the END of a page
    .byte   $60, $50, $40, $30, $20, $10, $00
    .byte   $f0, $e0, $d0, $c0, $b0, $a0, $90, $80
  IF (PASS > 1) & (<HMoveTbl >= $f1)
    ECHO ""
    ECHO "HMoveTbl aligned wrong!"
    ECHO ""
    ERR
  ENDIF

BcdTbl ; 7 values, up to $63 = 99
    .byte $00, $06, $12, $18, $24, $30, $36
;    .byte $42, $48, $54, $60, $66

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

  IF PLUSROM
PlusROM_API
    .byte   "a", 0, "h.firmaplus.de", 0
  ELSE
    ds      16, 0
  ENDIF

LVL_0   = 1    ; all must NOT divide by 4! (bonus lines)
LVL_1   = 7
LVL_2   = 14
LVL_3   = 22

VarLines
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

    COND_ALIGN_FREE_LBL GFX_H, 256, "PfColTbl"

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

; Dintari PacMan 8K:
; Siren : V=8; C=4; F=0e..1b, 1a..0f
; Scared: V=8; C/F = c/12, 4/1b, 12, 0d, 0a, 08, 07, 06
; Eyes  : V=8; C=4; F=06, 07, 08, 09, 0a, 0b, 0c, 0d, 0f, 11, 13, 15, 18, 1b, 1f

; Fruit : V=e; C=C; F=09, 0a, 0b, 0d, 0f, 12, 15, 18, 1d, 1f, 1d, 1b, 19, 17, 15, 12, 0f, 0d, 0b, 0a, 09, 08
; Death : V=e; C=4; F=06, 07, 08, 09, 0b, 0d, 10, 14, 1c ; C=C; F=0d, 1b   2x
;   11, 10, 0f, 0e, 0d, 0e, 0f, 10, 11, 12, 13, 14
;   13, 12, 11, 10, 0f, 10, 11, 12, 13, 14, 15, 16
;   15, 14, 13, 12, 11, 12, 13, 14, 15, 16, 17, 18
;   17, 16, 15, 14, 13, 14, 15, 16, 16, 18, 19, 1a
;   19, 18, 17, 16, 15, 16, 17, 18, 19, 1a, 1b, 1c
;   1b, 1a, 19, 18, 17, 18, 19, 1a, 1b, 1c, 1d, 1e

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
DEATH_END   = . - AudF0Tbl + 1
    .byte   0
;    .byte   $06, $07, $08, $09, $0b, $0d, $10, $14;, $1c, $1f ; $0d, $1b
    .byte   $06, $07, $08, $0a, $0d, $11, $16, $1d
DEATH_LEN2  = . - AudF0Tbl - DEATH_END
DEATH_END1  = DEATH_END + 2 * DEATH_LEN2
;final sounds missing here...
;    .byte   $1e, $1d, $1c, $1b, $1a, $19, $18, $17, $18, $19, $1a, $1b
;    .byte   $1c, $1b, $1a, $19, $18, $17, $16, $15, $16, $17, $18, $19
;    .byte   $1a, $19, $18, $17, $16, $15, $14, $13, $14, $15, $16, $17
;    .byte   $18, $17, $16, $15, $14, $13, $12, $11, $12, $13, $14, $15
;    .byte   $16, $15, $14, $13, $12, $11, $10, $0f, $10, $11, $12, $13
    .byte   $14, $13, $12, $11, $10, $0f, $0e, $0d, $0e, $0f, $10, $11
DEATH_LEN1  = . - AudF0Tbl - DEATH_END - DEATH_LEN2
DEATH_START = . + (6-1) * DEATH_LEN1 + (2-1) * DEATH_LEN2 - AudF0Tbl - 1

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
  IF HISCORE_DING
HISCORE_END = . - AudF1Tbl + 1
    .byte   0
HISCORE_START = . + DING_LEN + NUM_SHORT_DINGS * SHORT_DING_LEN - AudF1Tbl - 1

NUM_DINGS       = 10
NUM_SHORT_DINGS = NUM_DINGS - 1 ; 9 short dings and a long one
DingVolTbl
    .byte   0
    ds      8, 1
    ds      2, 2
ShortDingVol
    ds      2, 2
    ds      2, 3
    ds      2, 4
    .byte   5, 6, 8, 10, 12
DING_LEN    = . - DingVolTbl
SHORT_DING_LEN    = . - ShortDingVol
  ENDIF

Pot2Bit; 31x
    .byte   $01, $02, $04, $08, $10, $20, $40, $80
    CHECKPAGE Pot2Bit ; inside kernel (also frequently used)

; Bonus Scores:
; Pellet          10      1
; Power-Up        50      5
; Enemy          100     10
; Cherry         100     10
; Strawberry     300     30
; Orange         500     50         (aka Peach, Yellow Apple)
; Apple          700     70
; Melon         1000    100
; Grapes        2000    200         (Galaxian)
; Pear          3000    300         (Bell)
; Banana        5000    500         (Key)
BonusScore
    .byte   $10, $30, $50, $70;, $00, $00, $00, $00
BonusScoreHi
    .byte   $00, $00, $00, $00, $01, $02, $03, $05

  IF !PLUSROM
    .byte   "QUADTARI"
    .byte   "JTZ"

    ORG_FREE_LBL BASE_ADR + $ffc, "Vectors"
  ELSE
    ORG_FREE_LBL BASE_ADR + $ff0-1, "PlusROM Hotspots" ; next 4 bytes must not be accessed!

    .byte   "QUADTARI"
    .byte   "JTZ"

    ORG_FREE_LBL BASE_ADR + $ffa, "Vectors"
    .word PlusROM_API - BASE_ADR + $1000 ; must be $1xxx!
  ENDIF
Vectors
    .word   Start
    .word   VERSION


;===============================================================================
; O U T P U T
;===============================================================================

    LIST OFF
    ECHO ""
    ECHO "*** Free RAM   :", [$100 - STACK_SIZE - NUM_TMPS - RAM_END]d, "bytes ***"
    ECHO "*** Free ROM   :", [FREE_TOTAL + DEBUG_BYTES + COPYRIGHT_LEN]d, "bytes ***"
    ECHO ""
    ECHO "*** Debug bytes:", [DEBUG_BYTES]d, "bytes ***"
  IF COPYRIGHT_LEN
    ECHO "*** Copyright  :", [COPYRIGHT_LEN]d, "bytes ***"
  ENDIF

  IF SAVEKEY
    ECHO ""
    ECHO "*** SaveKey enabled! ***"
  ENDIF
  IF PLUSROM
    ECHO ""
    ECHO "*** PlusROM enabled! ***"
  ENDIF

