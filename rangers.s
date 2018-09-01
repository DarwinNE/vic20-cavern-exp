;                V I C   R A N G E R S
;
;             by Davide Bucci, August 2018


; General-use addresses
        GRCHARS1 = $1C00    ; Address of user-defined characters. Since in the
                            ; unexpanded VIC the screen matrix starts at
                            ; $1E00, there are 512 bytes free, i.e. 64 chars
                            ; that can be defined. That leaves 3059 bytes free
                            ; for the machine language code (counting the
                            ; 752 SYS4109 stub in BASIC that launches the
                            ; program. (3073 bytes?)

; Colour constants for the VIC 20
        BLACK    = $00
        WHITE    = $01
        RED      = $02
        CYAN     = $03
        MAGENTA  = $04
        GREEN    = $05
        BLUE     = $06
        YELLOW   = $07
        MULTICOLOUR = $08

; KERNAL routines used
        GETIN = $FFE4

; Page-0 addresses used (for indirect indexed addressing and other things)

        Val       = $0      ; Used for the BCD conversion (word)
        Res       = $2      ; The result of the BCD conversion (3 bytes)
        tmpindex1 = $5      ; Temporary variables (byte)
        tmpindex2 = $6      ; Temporary variables (byte)
        JustDrawn = $7
        ScreenPos = $8      ; The screen vertical position for VICSCRVE (b.)
        str1      = $9      ; Address of the string to print with PrintStr (w.)
        tmp4      = $B      ; Temporary (b.)
        ColourRead= $C      ; Colour read by GetChar (b.)
        POSCHARPT = $D      ; Pointer for a character in memory (w.)
        POSCOLPT  = $F      ; Pointer for a colour in memory (w.)

        SPRITECH  = $11      ; Pointer to the group of 4 ch. for a sprite (word)
        CHRPTR    = $13      ; Pointer to the original ch. in a sprite (word)
        SpriteX   = $15     ; X position (offset in a char) of a sprite (byte)
        SpriteY   = $16     ; Y position (offset in a char) of a sprite (byte)
        CharShr   = $17     ; Employed in LoadSprite (b.)
        CharCode  = $18     ; Employed in DrawChar (b.)
        PosX      = $19     ; (b.)
        PosY      = $1A     ; (b.)
        Colour    = $1B     ; Colour to be used by the printing routines (b.)
        CurrentCode=$1C     ; Code being processed for drawing walls (b.)
        Pos       = $1D     ; Used by PrintStr
        CavernPTR = $1E     ; Pointer to the cavern data (w.)
        CavernS   = $20     ; Size of the cavern (b.)
        CurrentYPos=$21     ; Position in the cavern (b.)
        ShipYSpeed= $22     ; Vertical speed of ship (relative to scroll, b.)
        IrqCn     = $23     ; Counter for the IRQ
        Period    = $24     ; Period for the IRQ events
        CavernPosX= $25     ; While drawing cavern, the hor. shift in chrs (b.)
        VertPosPx = $26     ; Vertical shift position in pixels of the ship (b.)
        ShipPosX  = $27     ; X position of the ship in the game in pixels (b.)
        ShipPosY  = $28     ; Y position of the ship in the game in pixels (b.)
        Win       = $29     ; Specify if the game should be stopped (b.)
        Joystick  = $2A     ; Different from zero if the joystick was used (b.)
        keyin     = $2B     ; Last key typed.
        ShipChrX=$2C        ; X position of the ship in characters (b.)
        ShipChrY=$2D        ; Y position of the ship in characters (b.)
        ShipXSpeed= $2E     ; Horisontal speed of the ship (b.)
        CurrXPosL = $2F     ; Current shift (in ch.) of the left wall (b.)
        CurrXPosR = $30     ; Current shift (in ch.) of the right wall (b.)
        CurrCode  = $31     ; Current code while drawing the cavern (b.)
        BorderCode= $32     ; Code to be used for the border (b.)
        Direction = $33     ; =0 if right border =128 if left border (b. cavern)
        SOURCE    = $34     ; Source pointer for CopyMem (w.)
        DEST      = $36     ; Destination pointer for CopyMem (w.)
        BLENDCHA  = $38     ; Saved ch. 1 to be blended with sprite 1
        BLENDCHB  = $39     ; Saved ch. 2 to be blended with sprite 1
        BLENDCHC  = $3A     ; Saved ch. 3 to be blended with sprite 1
        BLENDCHD  = $3B     ; Saved ch. 4 to be blended with sprite 1
        PlantShoot= $3C     ; Position of the plant shoot
        PlantDir  = $3D     ; Direction of the plant shoot (=0 or =128)
        RingState = $3E     ; bit 7: direction, bits 0 and 1: state
        Score     = $3F     ; Current score (w.)
        HiScore   = $41     ; High score (w.)
        CanIncLev = $42     ; =0 if the level can be incremented =128 otherwise
        Level     = $43     ; Current level number (b.)
        UpdateWPos= $44     ; =0 if the wall position has to be shifted (b.)
        IRQfreec  = $45     ; Free running counter sync with IRQ (b.)
        TmpPos    = $46     ; Temporary for PosChar (b.)
        PlantPos  = $47     ; Temporary for PutPlant1/2 (b.)
        GremlinsX = $48     ; X position of the gremlin (b.)
        GremlinsY = $49     ; Y position of the gremlin (b.)

        VoiceBase = $4E

        Voice1ptr = $4F
        Voice1ctr = $50
        Loop1ctr  = $51
        Loop1str  = $52
        Voice1drt = $53
        Voice1nod = $54

        Voice2ptr = $55
        Voice2ctr = $56
        Loop2ctr  = $57
        Loop2str  = $58
        Voice2drt = $59
        Voice2nod = $60


        INITVALC=$ede4

; VIC-chip addresses
        VICSCRHO = $9000    ; Horizontal position of the screen
        VICSCRVE = $9001    ; Vertical position of the screen
        VICCOLNC = $9002    ; Screen width in columns and video memory addr.
        VICROWNC = $9003    ; Screen height, 8x8 or 8x16 chars, scan line addr.
        VICRAST  = $9004    ; Bits 8-1 of the current raster line
        VICCHGEN = $9005    ; Character gen. and video matrix addresses.
        GEN1     = $900A    ; First sound generator
        GEN2     = $900B    ; Second sound generator
        GEN3     = $900C    ; Third sound generator
        NOISE    = $900D    ; Noise sound generator
        VOLUME   = $900E    ; Volume and additional colour info
        VICCOLOR = $900F    ; Screen and border colours

; VIA addresses
        PORTAVIA1  = $9111   ; Port A 6522 (joystick)
        ACRVIA1    = $911B
        PORTAVIA1d = $9113  ; Port A 6522 (joystick)
        T1CLVIA1   = $9114
        T1CHVIA1   = $9115
        T1LLVIA1   = $9116
        T1LHVIA1   = $9117
        IFRVIA1    = $911D
        IERVIA1    = $911E
        
        PORTBVIA2  = $9120   ; Port B 6522 2 value (joystick)
        PORTBVIA2d = $9122  ; Port B 6522 2 direction (joystick
        T1CLVIA2   = $9124
        T1CHVIA2   = $9125
        T1LLVIA2   = $9126
        T1LHVIA2   = $9127
        ACRVIA2    = $912B
        IFRVIA2    = $912D
        IERVIA2    = $912E

        MEMSCR   = $1E00    ; Start address of the screen memory (unexp. VIC)
        MEMCLR   = $9600    ; Start address of the colour memory (unexp. VIC)

        REPEATKE = $028A    ; Repeat all keys

        VOICE1  = GEN1      ; Voice 1 for music
        VOICE2  = GEN2      ; Voice 2 for music
        EFFECTS = GEN3      ; Sound effects (not noise)

.export main
.segment "STARTUP"
.segment "LOWCODE"
.segment "INIT"
.segment "GRCHARS"
.segment "CODE"

; Main program here.

main:       jsr Init        ; Init the game (load graphic chars, etc...)
restart:    jsr StartGame
mainloop:   lda #251
            sta IRQfreec
@wait:      lda IRQfreec
            bmi @wait
            jsr UpdateSpeedX
            jsr UpdateSpeedY
            jsr ValidatePos
            jsr GETIN       ; Main loop waiting for keyboard/joystick events
            sta keyin
            jsr CheckJoystick
            lda keyin
            beq mainloop
            cmp #$0D        ; Wait for return if the game stopped
            bne @norestart
            lda Win         ; If the game has stopped, restart
            bne restart
@norestart: lda keyin
            cmp #$58        ; X: right
            beq right
            cmp #$5A        ; Z: left
            beq left
            cmp #$53        ; S: up
            beq up
            cmp #$20        ; Space: fire!
            beq fire
            cmp #$4D        ; M toggle music on/off
            bne mainloop
            lda VoiceBase
            eor #$80
            sta VoiceBase
@continue4: jmp mainloop

up:         dec ShipPosY
            dec ShipPosY
            dec ShipYSpeed
            dec ShipYSpeed
            jmp mainloop

right:      inc ShipPosX
            inc ShipXSpeed
            inc ShipXSpeed
            jmp mainloop

left:       dec ShipPosX
            dec ShipXSpeed
            dec ShipXSpeed
            jmp mainloop

fire:       lda Win         ; If the game has stopped, restart
            bne restart
            jmp mainloop

UpdateSpeedX:
            lda ShipXSpeed
            beq @nobrake
            bmi @negative
            dec ShipXSpeed  ; Reduce the speed
            jmp @nobrake
@negative:  inc ShipXSpeed
@nobrake:   lda ShipPosX
            clc
            adc ShipXSpeed
            sta ShipPosX
            rts

UpdateSpeedY:
            lda ShipYSpeed
            beq @nobrake
            bmi @negative
            dec ShipYSpeed  ; Reduce the speed
            jmp @nobrake
@negative:  inc ShipYSpeed
@nobrake:   lda ShipPosY
            clc
            adc ShipYSpeed
            sta ShipPosY
@nospeed:   rts

ValidatePos:
            lda ShipPosY
            cmp #8
            bpl @ok
            lda #8
            sta ShipPosY
            lda #0
            sta ShipYSpeed
@ok:        rts

CheckJoystick:
            lda #$ff
            sta Joystick
            lda PORTAVIA1
            and #%00010000  ; Left
            beq left
            lda PORTBVIA2
            and #%10000000  ; Right
            beq right
            lda PORTAVIA1
            and #%00100000  ; Fire
            beq fire
            lda PORTAVIA1
            and #%00000100  ; Up
            beq up
            
            lda #$00
            sta Joystick
            rts
           
CorJoystick:
            lda Joystick
            beq @ccc
            jsr ShortDelay
            lda #$00
            sta Joystick
@ccc:       rts

StartGame:  lda #$2F        ; Turn on the volume, set multicolour add. colour 2
            sta VOLUME
            lda #4
            sta Period
            lda #$08
            sta VICCOLOR
            lda #$0
            sta CurrentYPos
            sta ShipXSpeed
            sta ShipYSpeed
            sta VertPosPx
            sta PlantShoot
            sta PlantDir
            sta Win
            sta ShipPosY
            sta Score
            sta Score+1
            sta CanIncLev
            sta Level
            lda #$80
            sta UpdateWPos
            sta GremlinX
            jsr CLSA
            lda #CYAN
            sta Colour
            lda #<MESSAGE
            sta str1
            lda #>MESSAGE
            sta str1+1
            ldx #5
            ldy #10
            jsr PrintStr
            ldx #4
            stx CurrXPosL
            ldx #11
            stx CurrXPosR
            lda #64
            sta ShipPosX
            rts

; Draw a cavern wall in the position specified in PosX, PosY

.macro OneLine
            tya
            adc #16
            tay
.endmacro


DrawCavern:
            ldy CurrentYPos
            ldx CavernPosX      ; Read the current position of the wall
            dex
            dex
            bit UpdateWPos      ; Skip the adjust of the position?
            bmi loop
            lda (CavernPTR),y   ; Read the first step and adjust the position 
            and #S_MASK         ; for the next run
            cmp #S_RIGH
            bne @nn2
            inc CavernPosX      ; Increment the x pos if there is S_RIGHT
            jmp loop
@nn2:       cmp #S_LEFT
            bne loop
            dec CavernPosX      ; Decrement the x pos if there is S_LEFT
loop:       sty tmpindex1       ; Start of the main loop
            lda (CavernPTR),y   ; Read the current step
            sta CurrCode
            ldy PosY            ; Get the current vertical position
            jsr PosChar
            ldy #2
            lda CurrCode        ; Handle the current decorations
            and #D_MASK         ; (rocks and skeletons in the walls)
            beq NoDecoration
            cmp #D_ROCK1
            bne @nd1
            jmp PutRock1
@nd1:       cmp #D_ROCK2
            bne @sk
            jmp PutRock2
@sk:        jmp PutSkeleton
NoDecoration:                   ; End of the decoration code
            lda CurrCode
            clv                 ; Clear the oVerflow bit (to use bvc as "bra")
            ; clc               ; The carry is clear after PosChar
            and #S_MASK
@no1:       cmp #S_RIGH         ; Code 01: StepRight+StepStay
            bcc @no0            ; If it is less than 01, it must be 00
            bne @no2
@goright:   jsr StepRight
            OneLine
            jsr StepStay
            bvc @cont           ; Branch always!
@no0:       jsr StepLeft        ; Code 00: StepLeft + StepRight
            OneLine
            jsr StepRight
            bvc @cont           ; Branch always!
@no2:       cmp #S_LEFT         ; Code 10: StepLeft+StepStay
            bne @no3
@goleft:    jsr StepLeft
            OneLine
            jsr StepStay
            bvc @cont           ; Branch always!
@no3:       jsr StepRight       ; Code 11: StepRight + StepLeft
            OneLine
            jsr StepLeft
@cont:      lda CurrCode        ; Handle plants, surface decorations, gremlins
            and #P_MASK
            beq NoSurface
            cmp #P_PLANT1
            bne @nd1
            jmp PutPlant1
@nd1:       cmp #P_PLANT2
            jmp PutPlant2
NoSurface:  lda CurrCode        ; Handle rings and levels
            and #L_MASK
            cmp #L_RING
            bne @nd2
            jmp PutRing
@nd2:       cmp #L_LEVEL
            bne NoRings
            jmp IncrementLevel
NoRings:
            lda PosY
            ;clc                ; The carry should be always clear here
            adc #4              ; Every step is 4 lines
            sta PosY
            cmp #30             ; Check if we got to the end of the screen
            bcs exitloop
            ldy tmpindex1
            iny
            jmp loop

exitloop:   
            rts


; The adc operation is done in StepStay, StepRight and StepLeft. However, it
; is expected that there is NEVER an overflow condition.

StepStay:   lda #WALL0A
            sta (POSCHARPT),Y
            lda Colour
            sta (POSCOLPT),Y
            OneLine
            lda #WALL0B
            sta (POSCHARPT),Y
            lda Colour
            sta (POSCOLPT),Y
            rts

StepRight:  lda #WALL1A
            sta (POSCHARPT),Y
            lda Colour
            sta (POSCOLPT),Y
            tya
            clc
            adc #17
            tay
            lda #WALL1B
            sta (POSCHARPT),Y
            lda Colour
            sta (POSCOLPT),Y
            inx
            rts

StepLeft:   lda #WALL2A
            sta (POSCHARPT),Y
            lda Colour
            sta (POSCOLPT),Y
            tya
            clc
            adc #15
            tay
            lda #WALL2B
            sta (POSCHARPT),Y
            lda Colour
            sta (POSCOLPT),Y
            dex
            rts

MoveBack:   bit Direction
            bmi @left
            iny
            iny
            rts
@left:      dey
            dey
            rts

MoveForward:
            bit Direction
            bmi @left
            dey
            dey
            rts
@left:      iny
            iny
            rts

; Put a first type of rocky "decoration" outside the walls. Colour depends
; on the number of level

PutRock1:   jsr MoveBack
            lda #ROCK1
            sta (POSCHARPT),Y
            lda Period
            sta (POSCOLPT),Y
            jsr MoveForward
            jmp NoDecoration

; Put a second type of rocky "decoration" outside the walls.

PutRock2:   lda #ROCK2
            jsr MoveBack
            sta (POSCHARPT),Y
            lda #CYAN
            sta (POSCOLPT),Y
            jsr MoveForward
            jmp NoDecoration

PutRing:    
            lda #RING
            jsr MoveForward
            sta (POSCHARPT),Y
            lda #CYAN
            sta (POSCOLPT),Y
            jsr MoveBack
            clc                 ; The code in DrawCavern expects carry clear
            jmp NoRings

PutSkeleton:
            lda #SKELETON1
            jsr MoveBack
            sta (POSCHARPT),Y
            lda #YELLOW
            sta (POSCOLPT),Y
            tya
            pha
            clc
            adc #16
            tay
            lda #SKELETON2
            sta (POSCHARPT),Y
            lda #YELLOW
            sta (POSCOLPT),Y
            pla
            tay
            jsr MoveForward
            jmp NoDecoration

PutPlant1:  lda #PLANT1
            sta (POSCHARPT),Y
            lda #GREEN
            sta (POSCOLPT),Y
            sty PlantPos
            tya
            iny
            adc PlantShoot
            tay
            bit PlantDir
            bmi @right
            lda #PLANTSHL
            jmp @cc
@right:     lda #PLANTSHR
@cc:        sta (POSCHARPT),Y
            lda #YELLOW
            sta (POSCOLPT),Y
            ldy PlantPos
            jmp NoRings

PutPlant2:  lda #PLANT2
            sta (POSCHARPT),Y
            lda #GREEN
            sta (POSCOLPT),Y
            sty PlantPos
            tya
            dey
            sec
            sbc PlantShoot
            clc
            tay
            bit PlantDir
            bmi @right
            lda #PLANTSHR
            jmp @cc
@right:     lda #PLANTSHL
@cc:        sta (POSCHARPT),Y
            lda #YELLOW
            sta (POSCOLPT),Y
            ldy PlantPos
            jmp NoRings

; INIT - INIT - INIT - INIT - INIT - INIT - INIT - INIT - INIT - INIT - INIT
;
; Initialization code: prepare the screen to the correct size, center it and
; load the graphic chars and configure the IRQ handler.
;
; INIT - INIT - INIT - INIT - INIT - INIT - INIT - INIT - INIT - INIT - INIT

SyncNTSC:
            ; Data for NTSC machines. See for example:
            ; http://www.antimon.org/dl/c64/code/stable.txt
            LINES_NTSC = 261
            CYCLES_PER_LINE_NTSC = 65
            TIMER_VALUE_NTSC = LINES_NTSC * CYCLES_PER_LINE_NTSC - 2
@loopsync:  lda VICRAST     ; Synchronization loop
            cmp #70
            bne @loopsync
            lda #<TIMER_VALUE_NTSC
            ldx #>TIMER_VALUE_NTSC
            jmp ContInit1


Init:       lda #$80        ; Autorepeat on on the keyboard
            sta REPEATKE
            lda #$08        ; Define screen colour and background
            sta VICCOLOR
            lda #$90        ; Set a 16 column-wide screen
            sta VICCOLNC
            lda #$7F        ; Set up the VIA 1 and 2 registers
            sta IERVIA2
            sta IFRVIA2
            sta IERVIA1
            sta IFRVIA1
            sta PORTBVIA2d  ; Prepare VIAs for joystick
            cmp #$05        ; Determine if we run on a PAL or NTSC machine
            lda INITVALC
            beq CenterScreenNTSC    ; Load the screen settings
            bne CenterScreenPAL
ContInit:   sty VICSCRVE    ; Centre the screen vertically...
            sty ScreenPos
            stx VICSCRHO    ; ... and horizontally
            lda #$FF        ; Move the character generator address to $1C00
            sta VICCHGEN    ; while leaving ch. 128-255 to their original pos.
            jsr MovCh       ; Load the graphic chars
            sei             ; Configure the interrupt handler
            lda #$40        ; Mode free run, output -> IRQ and NMI
            sta ACRVIA1     ; Set VIA registers
            sta ACRVIA2
            lda INITVALC
            cmp #$05
            beq SyncNTSC    ; Load the screen settings
            bne SyncPAL
ContInit1:  stx T1CHVIA2    ; Set up the timer and start it
            sta T1LLVIA2
            sta T1LLVIA1
            jsr SyncLater
            stx T1CHVIA1
            lda #<IrqHandler; The IRQ handler
            sta $0314
            lda #>IrqHandler
            sta $0315
            lda #<NMIHandler; The NMI handler
            sta $0318
            lda #>NMIHandler
            sta $0319
            lda #$c0
            sta IERVIA2
            lda #$c0
            sta IERVIA1
            cli
            lda #0
            sta HiScore     ; Zero the hi-score
            sta HiScore+1
            sta PORTAVIA1d  ; Prepare VIAs for joystick
            rts

; Synchronize raster on PAL systems

SyncPAL:
            ; Data for PAL machines. See for example:
            ; http://www.antimon.org/dl/c64/code/stable.txt
            LINES_PAL = 312
            CYCLES_PER_LINE_PAL = 71
            TIMER_VALUE_PAL = LINES_PAL * CYCLES_PER_LINE_PAL - 2
@loopsync:  lda VICRAST     ; Synchronization loop
            cmp #140
            bne @loopsync
            lda #<TIMER_VALUE_PAL
            ldx #>TIMER_VALUE_PAL
            jmp ContInit1

; Synchronize the timer to the NMI interrupt to a given raster line.

SyncLater:
            lda #20     ; Synchronization loop
@loopsync:  cmp VICRAST
            bne @loopsync
            rts

; Screen init value for PAL and NTSC

CenterScreenPAL:
            lda #$3E        ; Set a 31 row-high column
            sta VICROWNC
            ldx #$12
            ldy #$16
            jmp ContInit

CenterScreenNTSC:
            lda #$36        ; Set a 27 row-high column
            sta VICROWNC
            ldx #$0A
            ldy #$10
            jmp ContInit


; Copy the graphic chars. They are subjected to be changed during the pixel-by
; pixel movement, so that routine moves only the characters not used as sprites
; Since the counter is the 8-bit register x, this routine can move a maximum
; of 30 characters.

MovCh:      ldx #(LASTCH+1)*8+1
@loop:      lda DefChars-1,x
            sta GRCHARS1-1,x
            dex
            bne @loop
            rts

; Print the values contained in Res+2, Res+1, Res.

PrintRes:   sta Colour
            ldy #0
            lda Res+2       ; Print all the BCD chars
            jsr PrintBCD
            lda Res+1
            jsr PrintBCD
            lda Res
            jmp PrintBCD
            

; Write 16 characters vertically start from the current position of
; POSCHARPT and put red in the corresponding colour address.

Vert16car:  ldx #16
@loop1:     lda BorderCode
            sta (POSCHARPT),Y
            lda #RED
            sta (POSCOLPT),Y
            tya
            clc
            adc #16
            tay
            dex
            bne @loop1
            rts

DrawHalfBorder:
            ldy #0
            lda #BORDER1L
            sta BorderCode
            jsr Vert16car
            ldy #0
            lda #BORDER2L
            sta BorderCode
            jsr Vert16car
            ldy #15
            lda #BORDER2R
            sta BorderCode
            jsr Vert16car
            ldy #15
            lda #BORDER1R
            sta BorderCode
            jmp Vert16car
            
DrawBorder: rts
            
            ldy #0
            ldx #0
            jsr PosChar
            jsr DrawHalfBorder
            ldy #15
            ldx #0
            jsr PosChar
            jmp DrawHalfBorder

; NMI - NMI - NMI - NMI - NMI - NMI - NMI - NMI - NMI - NMI - NMI - NMI - NMI
;
; This is the NMI handler, called 50 times each second when the VIC-20
; is a PAL unit or 60 when NTSC.
;
; NMI - NMI - NMI - NMI - NMI - NMI - NMI - NMI - NMI - NMI - NMI - NMI - NMI

NMIHandler: pha
            lda #$12
            sta VICSCRHO
            ;lda #$0A
            ;sta VICCOLOR
@nodrawship:
            bit T1CLVIA1
            pla
            rti

; IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ
;
; This is the interrupt handler, called 50 times each second when the VIC-20
; is a PAL unit or 60 when NTSC. It does the following things:
;
;
; IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ - IRQ
vpos=$20
IrqHandler: pha
            txa             ; Save registers
            pha
            tya
            pha

            lda #08
            sta VICCOLOR
            lda #63         ; Switch "off" the screen, shifting all to right
            sta VICSCRHO
            ;lda #5
            ;sta VICSCRVE
            inc IRQfreec
            lda Win
            beq @nowin
            jmp @cont3
@nowin:
            dec IrqCn
            ;lda IrqCn
            ;cmp Period      ; Execute every PERIOD/60 of second
            beq @contint
            jmp @redraw
@contint:   lda Period          ; Restart the counter for the period
            sta IrqCn
            inc VertPosPx   ; Since the screen will be scrolled upwards,
            inc VertPosPx   ; compensate the position of the ship by 2 pixels
            inc ShipPosY    ; Increment the position of the ship (with respect
            lda ShipPosY    ; to the field) up to line 127. Fall down by gravity
            cmp #127
            bmi @nopb       ; Check if we past 127th line.
            lda #127
            sta ShipPosY
@nopb:      dec ScreenPos   ; Decrement the screen position so that everything
            lda ScreenPos   ; scrolls towards the top by 2 raster lines.
            sta VICSCRVE
            
            ;beq @redraw     ; branch always
            cmp #vpos-16    ; 16 x 2 pixels = 4 lines
            bne @redraw
            lda #vpos       ; Put the screen again in the bottom position
            sta ScreenPos
            sta VICSCRVE
            inc CurrentYPos ; We increase the position in the cavern
            dec GremlinsY
            lda #0
            sta VertPosPx   ; We also put to 0 the shift in the ship position
            lda CanIncLev   ; And we decrement the counter for the dead time
            beq @redraw     ; for incrementing the level
            dec CanIncLev
@redraw:    lda ScreenPos
            cmp #vpos-15
            bne @chk
            lda IrqCn
            cmp #1
            bne @chk        ; Here we must prepare the shift in the walls for
            lda #$00        ; the next step.
            sta UpdateWPos
            jsr UpdatePlants
            jsr ChangeRingState
@chk:       lda #EMPTY      ; Launch a complete redraw of the cavern
            jsr CLS
            jsr DrawBorder
            
                            ; Prepare for the left wall
            lda #<CavernLeft
            sta CavernPTR
            lda #>CavernLeft
            sta CavernPTR+1
            lda #WHITE
            sta Colour
            ldy #0
            sty PosY
            lda CurrXPosL
            sta CavernPosX
            lda #128
            sta Direction
            clc             ; TO DO: check if useful!!!
            jsr DrawCavern  ; Draw the left wall of the cavern
            lda CavernPosX
            sta CurrXPosL
                            ; Prepare for the right wall
            lda #<CavernRight
            sta CavernPTR
            lda #>CavernRight
            sta CavernPTR+1
            ldy #0
            sty PosY
            lda CurrXPosR
            sta CavernPosX
            lda #0
            sta Direction
            jsr DrawCavern
            lda CavernPosX
            sta CurrXPosR
            lda #128
            sta UpdateWPos
            sta JustDrawn
@endscroll:
@cont3:     lda Win
            bne @nodrawship
            jsr DrawGremlin
            jsr DrawShip
@nodrawship:

@nomusic1:  ;lda #$0A
            ;sta VICCOLOR
            pla             ; Restore registers
            tay
            pla
            tax
            pla
            jmp $EABF       ; Jump to the standard IRQ handling routine

UpdatePlants:
            lda PlantDir    ; Update the position of plant shoots
            bmi @negshoot
            inc PlantShoot
            lda PlantShoot
            cmp #4
            bne @nochange   ; Change the direction of the shoot
            lda #128
            sta PlantDir
            jmp @nochange
@negshoot:  dec PlantShoot
            lda PlantShoot
            cmp #0
            bne @nochange
            lda #0
            sta PlantDir
@nochange:  rts

; Change the ring state in the following sequence:
; 0 - 1 - 2 - 3 - 2 - 1  and then restart.

ChangeRingState:
            bit RingState       ; Test the direction of the ring state sequence
            bmi @decrease
            inc RingState       ; Increase
            lda RingState
            cmp #$3             ; If we get to 3, change direction
            bne @exit
            lda #($80+$3)
            sta RingState
            bne @exit           ; Branch always
@decrease:  dec RingState       ; Decrease
            cmp #$80
            bne @exit           ; If we get to 0, change direction
            lda #$0
            sta RingState
@exit:                          ; No rts here!

; Load the current shape of the ring in the generic character, following the
; value of RingState (from 0 to 3).

            and #$03
            clc
            adc #RING1
            tax
            ldy #RING
            jmp CopyChar
            

CheckCrash: 
            ldx ShipChrX
            ldy ShipChrY
            jsr GetChar
            cmp #SPRITE1A
            beq @skip
            sta BLENDCHA
            iny
            jsr GetChar
            sta BLENDCHB
            dey
            inx
            jsr GetChar
            sta BLENDCHC
            iny
            jsr GetChar
            sta BLENDCHD

@skip:      
            ldx BLENDCHA
            cpx #EMPTY
            beq @next1
            cpx #SPRITE1A
            beq @next4
            ldy #SPRITE1A
            jsr CheckCollision
@next1:     ldx BLENDCHB
            cpx #EMPTY
            beq @next2
            ldy #SPRITE1B
            jsr CheckCollision
@next2:     ldx BLENDCHC
            cpx #EMPTY
            beq @next3
            ldy #SPRITE1C
            jsr CheckCollision
@next3:     ldx BLENDCHD
            cpx #EMPTY
            beq @next4
            ldy #SPRITE1D
            jmp CheckCollision
@next4:     rts

IncrementLevel:
            lda #('L'-'@')  ; Show L followed by the number of the level
            sta (POSCHARPT),Y
            lda Level
            adc #(48+$80) ; Convert into a number
            iny
            sta (POSCHARPT),Y
            dey
            lda CanIncLev
            bne @normal
            lda #10
            sta CanIncLev
            inc Level
            dec Period
            lda Period
            cmp #0
            bne @normal
            inc Period
@normal:    clc
            jmp NoRings

DrawGremlin:
            bit GremlinX
            bmi @exit
            ldx GremlinX
            ldy GremlinY
            cpy #0
            beq @@deactivate
            lda #BLUE
            sta Colour
            lda #GREMLIN
            jmp DrawChar
@deactivate:
            lda #80
            sta GremlinX
@exit:      rts

; Draw the player's ship

DrawShip:   lda ShipPosX    ; Calculate the ship positions in characters
            and #7
            sta SpriteX
            lda VertPosPx
            clc
            adc ShipPosY
            sta tmp4
            and #7
            sta SpriteY
            lda ShipPosX
            lsr
            lsr
            lsr
            tax
            lda #3          ; The line that correspond to highest ship pos.
            lsr tmp4
            lsr tmp4
            lsr tmp4
            clc
            adc tmp4
            tay             ; X and Y now contain the new positions of the ship
            sty ShipChrY
            stx ShipChrX
            ;cpy ShipChrY
            ;jmp EraseOldShip
            ;cpx ShipChrX
            ;bne EraseOldShip
NormalShip: lda #SHIP
            sta CharCode
            lda #WHITE
            sta Colour
            jsr LoadAppropriateSprite
            jsr CheckCrash
            jsr BlendSprite
            ; lda #SHIP
;             sta CharCode
;             lda #<(GRCHARS1+SPRITE1A*8)
;             sta SPRITECH
;             lda #>(GRCHARS1+SPRITE1A*8)
;             sta SPRITECH+1
;             ;jsr LoadSprite
            jmp DrawSprite

EraseOldShip:
            tya             ; Save the positions in X and Y that become the
            ldy ShipChrY    ; stored positions in ShipChrX and ShipChrY
            sta ShipChrY
            txa
            ldx ShipChrX
            sta ShipChrX
            jmp NormalShip
            bit JustDrawn
            bmi @exit
            lda #EMPTY
            dey
            jsr DrawChar
            inx
            jsr DrawChar
            iny
            dex
            lda BLENDCHA
            jsr DrawChar    ; X and Y still contain the old positions, though
            iny
            lda BLENDCHB
            jsr DrawChar
            dey
            inx
            lda BLENDCHC
            jsr DrawChar
            iny
            lda BLENDCHD
            jsr DrawChar
            dey
            lda #0
            sta JustDrawn
@exit:      jmp NormalShip

; Draw the current sprite at the ShipChrX, ShipChrY position

DrawSprite: ldx ShipChrX
            ldy ShipChrY
            lda #SPRITE1A
            jsr DrawCharFast
            iny
            lda #SPRITE1B
            jsr DrawCharFast
            lda #SPRITE1C
            inx
            dey
            jsr DrawCharFast
            lda #SPRITE1D
            iny
            jmp DrawCharFast

LoadAppropriateSprite:
            lda #<(GRCHARS1+SPRITE1A*8)
            sta SPRITECH
            lda #>(GRCHARS1+SPRITE1A*8)
            sta SPRITECH+1
            jsr ClearSprite
            jsr CalcChGenOfs
            jmp LoadSprite

; Check a possible collision between character x and y.
; Employs CharCode, CHARPTR, DEST, registers A, X, Y, Win.

CheckCollision:
            stx CharCode
            jsr CalcChGenOfs
            lda CHRPTR
            sta DEST
            lda CHRPTR+1
            sta DEST+1

            sty CharCode
            jsr CalcChGenOfs

            ldy #7
@loop:      lda (CHRPTR),Y
            and (DEST),Y
            bne Collision
            dey
            cpy #$FF
            bne @loop
            rts
Collision:  cpx #RING
            bne Die
            lda #1
            jmp AddScore


; Stop the game, explode the ship and die :-(
Die:        lda #$FF
            sta Win         ; Stop the game
            sta VICCOLOR    ; Light yellow screen, yellow border
            lda #EXPLOSION1
            sta CharCode
            lda #MULTICOLOUR
            sta Colour
            jsr LoadAppropriateSprite
            jsr DrawSprite
            jsr ShortDelay
            lda #125        ; Yellow screen, green border
            sta VICCOLOR
            jsr ShortDelay
            lda #42         ; Red screen, red border
            sta VICCOLOR
            jsr ShortDelay
            lda #$A         ; Black screen, red border
            sta VICCOLOR

; Add the value contained in A to the current score
AddScore:   clc
            adc Score
            sta Score
            bcc @contadd
            inc Score+1
@contadd:   rts

; Draw the character in A in the position given by the X and Y registers
; Since the screen is 16 characters wide, we need to shift the Y register
; to multiply times 16 and then add the X contents. It uses PosX and PosY.
; Colour contains the colour code of the character. It uses 1 byte in the
; stack and does not change A, X, Y.

DrawChar:   cpx #16         ; Check if the X value is out of range
            bcs _exit       ; Exit if X greater than 16 (no of columns)
            cpy #31         ; Check if the Y value is out of range
            bcs _exit       ; Exit if Y greater than 31 (no of rows)
DrawCharFast:               ; Skip the controls :-)
            sty PosY
            pha
            jsr PosChar
            ldy #0
            lda Colour
            sta (POSCOLPT),Y
            pla
            sta (POSCHARPT),Y
            ldy PosY
_exit:      rts

; Get the screen code of the character in the X and Y locations.
; The character is returned in A. The character colour is returned in
; ColourRead

GetChar:    cpx #16         ; Check if the X value is out of range
            bcs @exit       ; Exit if X greater than 16 (no of columns)
            cpy #31         ; Check if the Y value is out of range
            bcs @exit       ; Exit if Y greater than 31 (no of rows)
            sty PosY
            stx PosX
            lda #<MEMSCR
            sta POSCHARPT
            lda #>MEMSCR
            sta POSCHARPT+1
            tya
            asl             ; 16 columns per line. Multiply!
            asl
            asl
            asl             ; If it shifts an 1 in the carry, this means that
            bcc @nocorr     ; we need to write in the bottom-half of the screen
            inc POSCHARPT+1
            clc
@nocorr:    adc PosX
            adc POSCHARPT
            sta POSCHARPT
            ldy #0
            lda (POSCOLPT),Y
            sta ColourRead
            lda (POSCHARPT),Y
            ldy PosY
            rts
@exit:      lda #EMPTY
            rts

; Calculate the address of a screen position and put it in POSCHARPT. Do the
; same for the color address and put it in POSCOLPT.
; X and Y contain screen coordinates.

PosChar:    stx PosX
            lda #<MEMSCR
            sta POSCHARPT
            lda #>MEMSCR
            sta POSCHARPT+1
            lda #<MEMCLR
            sta POSCOLPT
            lda #>MEMCLR
            sta POSCOLPT+1
            tya
            asl             ; 16 columns per line. Multiply!
            asl
            asl
            asl             ; If it shifts an 1 in the carry, this means that
            bcc @nocorr     ; we need to write in the bottom-half of the screen
            inc POSCHARPT+1
            inc POSCOLPT+1
            clc
@nocorr:    adc PosX
            sta TmpPos
            ;pha
            adc POSCHARPT
            sta POSCHARPT
            lda TmpPos
            ;pla
            adc POSCOLPT
            sta POSCOLPT
            rts


; Put a "sprite", that is a 8x8 cell in a 2x2 character position.
; A contains the character code. It should be less than 32.
; The 4 characters are disposed as follows:
;
;    AC
;    BD
;
; Characters are redefined starting from address contained in SPRITECH
; SpriteX: the horizontal offset in pixels [0,8]
; SpriteY: the vertical offset in pixels [0,8]
; CharCode: the character to be used for the sprite
; Employs SPRITECH pointer to the group of 4 ch. for a sprite (word)
; and CHRPTR pointer to the original ch. in a sprite (word).

LoadSprite: clc
            lda SPRITECH        ; Calculate the vert. offset in ch. table
            adc SpriteY
            sta SPRITECH
            bcc @normal         ; Correct if page change
            inc SPRITECH+1
@normal:    jsr CalcChGenOfs
            ldy #0              ; Copy 8 bytes
@loop1:     lda #0
            sta CharShr
            lda (CHRPTR),y      ; Charge source
            ldx SpriteX
            beq @noshift
@loop2:     lsr                 ; At the beginning of the cycle, x contains
            ror CharShr         ; the shift in pixels to the right
            dex
            bne @loop2
@noshift:   ora (SPRITECH),y
            sta (SPRITECH),y    ; Save
            tya
            pha
            adc #16
            tay
            lda CharShr
            ora (SPRITECH),y
            sta (SPRITECH),y    ; Save
            pla
            tay
            iny
            cpy #08
            bne @loop1
            rts

; Load CHRPTR and CHRPTR+1 with the address of the character generator area
; corresponding to the character contained in CharCode.
; Works only up to 32 characters.
; Use CharCode, CHRPTR and register A

CalcChGenOfs:
            lda #<GRCHARS1      ; Charge the address of the ch. gen in CHARPTR
            sta CHRPTR
            lda #>GRCHARS1
            sta CHRPTR+1
            lda CharCode        ; Charge the ch. code to be used
            asl                 ; Multiply it times 8
            asl
            asl
            ;clc                ; We are sure the carry is clear if CharCode<32
            adc CHRPTR          ; Add to the CHRPTR (to get address of the ch.)
            sta CHRPTR
            bcc @normal         ; Correct if page change
            inc CHRPTR+1
@normal:    rts

; Clear the contents of a "sprite".
ClearSprite:
            lda #0
            ldy #32
@loop:      dey
            sta (SPRITECH),y    ; sta does not affect processor flags
            bne @loop
            rts

; Prepare the background of the first sprite with the code.
BlendSprite:
            ldx BLENDCHA
            ldy #SPRITE1A
            jsr OrChar
            ldx BLENDCHB
            ldy #SPRITE1B
            jsr OrChar
            ldx BLENDCHC
            ldy #SPRITE1C
            jsr OrChar
            ldx BLENDCHD
            ldy #SPRITE1D
            ;jmp CopyChar       ; no rts or jmp here

OrChar:     stx CharCode
            tya
            pha
            jsr CalcChGenOfs
            lda CHRPTR
            sta SOURCE
            lda CHRPTR+1
            sta SOURCE+1
            
            pla
            sta CharCode
            jsr CalcChGenOfs
            lda CHRPTR
            sta DEST
            lda CHRPTR+1
            sta DEST+1
            ldy #8
            ;                   ; no rts here
OrMem:      dey
            lda (SOURCE),y
            ora (DEST),y
            sta (DEST),y
            cpy #0
            bne OrMem
            rts
; Prepare for a copy of the memory of the character to be blended in the sprite
; 1 area.
; X = the caracter to be blended (source)
; Y = the sprite character (destination)
CopyChar:   stx CharCode
            tya
            pha
            jsr CalcChGenOfs
            lda CHRPTR
            sta SOURCE
            lda CHRPTR+1
            sta SOURCE+1

            pla
            sta CharCode
            jsr CalcChGenOfs
            lda CHRPTR
            sta DEST
            lda CHRPTR+1
            sta DEST+1
            ldy #8
            ;                   ; no rts here
CopyMem:    dey
            lda (SOURCE),y
            sta (DEST),y
            cpy #0
            bne CopyMem
            rts
            
; Print a string (null terminated) whose address is contained in str1 and
; str1+1 at the position given by X and Y pointers

PrintStr:   sty Pos
            ldy #$00
@loop:      lda (str1),Y
            beq @exit
            sty tmp4
            ldy Pos
            jsr DrawChar
            ldy tmp4
            iny
            inx
            jmp @loop
@exit:      ldy Pos
            rts


; Clear the screen and set the colour to black.

CLSA:
            lda #BLACK
            jsr PaintColour
            lda #EMPTY
            ; no return here!

; Fill the screen.
; Draw everywhere the character contained in the A register. Employs X.

CLS:
            size=16*31/16+1
            ldx #size
@loop:      sta MEMSCR-1,X          ; A (small) degree of loop unrolling avoids
            sta MEMSCR+size-1,X     ; to mess with a 16-bit loop.
            sta MEMSCR+size*2-1,X
            sta MEMSCR+size*3-1,X
            sta MEMSCR+size*4-1,X
            sta MEMSCR+size*5-1,X
            sta MEMSCR+size*6-1,X
            sta MEMSCR+size*7-1,X
            sta MEMSCR+size*8-1,X 
            sta MEMSCR+size*9-1,X
            sta MEMSCR+size*10-1,X
            sta MEMSCR+size*11-1,X
            sta MEMSCR+size*12-1,X
            sta MEMSCR+size*13-1,X
            sta MEMSCR+size*14-1,X
            sta MEMSCR+size*15-1,X
            dex
            bne @loop
            rts

; Put the colour code contained in A everywhere in the screen

PaintColour:
            ldx #size
@loop:      sta MEMCLR-1,X
            sta MEMCLR+size-1,X
            sta MEMCLR+size*2-1,X
            sta MEMCLR+size*3-1,X
            sta MEMCLR+size*4-1,X
            sta MEMCLR+size*5-1,X
            sta MEMCLR+size*6-1,X
            sta MEMCLR+size*7-1,X
            dex
            bne @loop
            rts

; A simple delay

Delay:      ldx #$FF
            ldy #$FF
Delayloop:  dex
            bne Delayloop
            dey
            bne Delayloop
            rts

ShortDelay: ldy #$40
            ldx #$FF
            jmp Delayloop

; Convert a 16-bit word to a 24-bit BCD. Adapted from here:
; http://www.obelisk.me.uk/6502/algorithms.html
; I like how it is compact and the clever use of the BCD mode of the 6502

; Convert an 16 bit binary value into a 24bit BCD value
Bin2BCD:    lda #0          ; Clear the result area
            sta Res+0
            sta Res+1
            sta Res+2
            ldx #16         ; Setup the bit counter
            sed             ; Enter decimal mode
@loop:      asl Val+0       ; Shift a bit out of the binary
            rol Val+1       ; ... value
            lda Res+0       ; And add it into the result, doubling
            adc Res+0       ; ... it at the same time
            sta Res+0
            lda Res+1
            adc Res+1
            sta Res+1
            lda Res+2
            adc Res+2
            sta Res+2
            dex             ; More bits to process?
            bne @loop
            cld             ; Leave decimal mode
            rts

; Print the BCD value in A as two ASCII digits

PrintBCD:   pha             ; Save the BCD value
            lsr             ; Shift the four most significant bits
            lsr             ; ... into the four least significant
            lsr
            lsr
            clc
            adc #(48+$80)   ; Make a screen code char
            jsr DrawChar
            inx
            pla             ; Recover the BCD value
            and #$0F        ; Mask out all but the bottom 4 bits
            clc
            adc #(48+$80)   ; Make an screen code char
            jsr DrawChar
            inx
            rts


; DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA
;
; Data and configuration settings. Tables and some music-related stuff.
; Characters!
;
; DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA

; Cavern data

; Two bits specify a code:
; 00 StepStay
; 01 StepRight
; 10 StepLeft
; 11 StepRight+StepLeft (wiggle)

S_MASK = %00000011
S_RIGH = %00000001
S_LEFT = %00000010
S_STAY = %00000000
S_WIGG = %00000011

D_MASK  = %00001100
D_ROCK1 = %00000100
D_ROCK2 = %00001000
D_SKEL  = %00001100

P_MASK   = %00110000
P_PLANT1 = %00010000
P_PLANT2 = %00100000
P_MONST  = %00110000

L_MASK = %11000000
L_RING = %01000000
L_LEVEL= %10000000

CAVERNLENR = 255
CavernRight:
            .byte S_RIGH,S_RIGH+D_ROCK1,S_STAY,S_LEFT,S_WIGG+D_ROCK1
            .byte S_STAY,S_STAY+L_RING,S_WIGG ;+1
            .byte S_LEFT+D_ROCK1+L_RING,S_RIGH+L_RING,S_STAY+D_ROCK1
            .byte S_LEFT,S_WIGG+P_PLANT2
            .byte S_STAY+D_ROCK1+L_RING,S_STAY,S_RIGH ; 0
            .byte S_WIGG,S_RIGH+D_ROCK1+L_RING,S_STAY,S_LEFT
            .byte S_WIGG,S_STAY+D_ROCK1,S_STAY,S_RIGH ;+1
            .byte S_RIGH+D_ROCK1+L_RING,S_LEFT+P_PLANT2,S_STAY,S_RIGH+D_ROCK2
            .byte S_WIGG+L_RING,S_STAY+D_ROCK1,S_STAY+D_ROCK1,S_LEFT+L_LEVEL ; 0
            .byte S_LEFT+L_RING,S_LEFT+D_ROCK1,S_STAY,S_RIGH+D_ROCK1
            .byte S_WIGG,S_STAY+D_ROCK1+L_RING,S_STAY
            .byte S_WIGG+P_PLANT2 ; -1
            .byte S_RIGH+L_RING,S_LEFT+D_ROCK1,S_STAY,S_RIGH
            .byte S_WIGG+P_PLANT2+D_ROCK1
            .byte S_STAY+L_RING,S_STAY+L_RING,S_LEFT+P_PLANT2 ; 0
            .byte S_WIGG+D_ROCK1+L_RING,S_LEFT,S_STAY+D_ROCK1+L_RING
            .byte S_RIGH+D_ROCK2
            .byte S_WIGG,S_STAY+D_ROCK1+L_RING,S_STAY,S_LEFT+P_PLANT2 ; -1
            .byte S_RIGH+D_ROCK1,S_LEFT+L_RING,S_STAY+D_ROCK1
            .byte S_RIGH,S_WIGG+P_PLANT2
            .byte S_STAY+D_ROCK1+L_RING,S_STAY+L_RING,S_LEFT ; 0
            
            .byte S_WIGG+D_ROCK1+L_RING,S_RIGH+D_ROCK2,S_STAY+D_ROCK1,S_LEFT
            .byte S_WIGG+P_PLANT2+D_ROCK1
            .byte S_STAY+D_ROCK1+L_RING,S_STAY+D_ROCK2,S_RIGH+L_LEVEL ; +1
            .byte S_RIGH+D_ROCK2,S_LEFT+D_ROCK1+L_RING,S_STAY
            .byte S_RIGH+D_ROCK1,S_WIGG
            .byte S_STAY,S_STAY+D_ROCK1,S_LEFT ; 0
            .byte S_WIGG+D_ROCK1+L_RING,S_LEFT+D_ROCK2,S_STAY,S_RIGH+D_ROCK2
            .byte S_WIGG+P_PLANT2+D_ROCK1
            .byte S_STAY+D_ROCK1+L_RING,S_STAY+L_RING,S_LEFT+D_ROCK2 ; -1
            .byte S_RIGH+D_ROCK2,S_LEFT+D_ROCK1+L_RING,S_STAY+D_ROCK1
            .byte S_RIGH,S_WIGG+D_ROCK1,S_STAY+L_RING,S_STAY+D_ROCK2
            .byte S_LEFT+D_ROCK1+L_RING ; 0
            .byte S_RIGH,S_RIGH+D_ROCK2+L_RING,S_STAY+D_ROCK1,S_LEFT
            .byte S_WIGG,S_STAY+D_ROCK2+L_RING
            .byte S_STAY,S_WIGG+L_RING ;+1
            .byte S_LEFT+D_ROCK1,S_RIGH+L_RING,S_STAY+D_ROCK2
            .byte S_LEFT+D_ROCK1+L_RING,S_WIGG+P_PLANT2
            .byte S_STAY+D_ROCK1,S_STAY+D_ROCK1,S_RIGH ; 0
            .byte S_WIGG,S_RIGH+D_ROCK1,S_STAY+L_RING,S_LEFT,S_WIGG
            .byte S_STAY+D_ROCK2,S_STAY+L_RING,S_RIGH+D_ROCK2 ;+1
            .byte S_RIGH+D_ROCK1,S_LEFT+P_PLANT2+D_ROCK2,S_STAY
            .byte S_RIGH,S_WIGG,S_STAY+L_RING
            .byte S_STAY+D_ROCK2,S_LEFT+D_ROCK1+L_LEVEL ; 0
            
            .byte S_LEFT+D_ROCK1,S_LEFT+L_RING,S_STAY+D_ROCK1,S_RIGH+D_ROCK2
            .byte S_WIGG+P_PLANT2+D_ROCK1,S_STAY+L_RING
            .byte S_STAY+L_RING,S_WIGG+P_PLANT2+D_ROCK2 ; -1
            .byte S_RIGH+D_ROCK2+L_RING,S_LEFT+D_ROCK1,S_STAY,S_RIGH+D_ROCK2
            .byte S_WIGG+D_ROCK2+L_RING
            .byte S_STAY+D_ROCK2,S_STAY+D_ROCK1+L_RING,S_LEFT+D_ROCK2 ; 0
            .byte S_WIGG+D_ROCK1,S_LEFT+D_ROCK2,S_STAY+L_RING,S_RIGH+D_ROCK2
            .byte S_WIGG+P_PLANT2
            .byte S_STAY+D_ROCK2+L_RING,S_STAY+D_ROCK1,S_LEFT ; -1
            .byte S_RIGH+D_ROCK1,S_LEFT+L_RING,S_STAY,S_RIGH+D_ROCK2
            .byte S_WIGG+P_PLANT2
            .byte S_STAY+D_ROCK1,S_STAY+L_RING,S_LEFT+D_ROCK2 ; 0
            .byte S_RIGH+D_ROCK2,S_RIGH+D_ROCK1+L_RING,S_STAY+D_SKEL
            .byte S_LEFT,S_WIGG+D_ROCK1+L_RING,S_STAY+D_ROCK1
            .byte S_STAY+D_ROCK2,S_WIGG ;+1
            .byte S_LEFT,S_RIGH+D_ROCK1+L_RING,S_STAY,S_LEFT+D_ROCK2
            .byte S_WIGG+P_PLANT2
            .byte S_STAY+D_ROCK1,S_STAY+D_ROCK2,S_RIGH+D_ROCK1 ; 0
            .byte S_WIGG+D_ROCK2+L_RING,S_RIGH
            .byte S_STAY+D_ROCK1,S_LEFT+P_PLANT2+D_ROCK1
            .byte S_WIGG+D_ROCK1,S_STAY+D_ROCK1+L_RING
            .byte S_STAY,S_RIGH+D_ROCK2+L_RING ;+1
            .byte S_RIGH,S_LEFT+D_ROCK1+L_RING,S_STAY+D_ROCK1
            .byte S_RIGH+P_PLANT2+D_ROCK2+L_RING
            .byte S_WIGG+P_PLANT2+L_RING,S_STAY+D_ROCK2+D_ROCK1
            .byte S_STAY+D_ROCK1+L_RING,S_LEFT+D_ROCK2 ; 0

            .byte S_WIGG+D_ROCK1+L_RING,S_LEFT+D_ROCK2+P_PLANT2,S_STAY
            .byte S_RIGH+D_ROCK1+L_RING,S_WIGG+L_RING
            .byte S_STAY+D_ROCK1+L_RING,S_STAY+D_ROCK2+P_PLANT2
            .byte S_LEFT+L_LEVEL ; -1
            .byte S_LEFT+D_ROCK1+P_PLANT2,S_RIGH+L_RING,S_STAY
            .byte S_LEFT,S_WIGG+D_ROCK1
            .byte S_STAY+P_PLANT2,S_STAY+D_ROCK2+P_PLANT2,S_RIGH+L_RING ; 0
            .byte S_WIGG+D_ROCK2+L_RING,S_RIGH+P_PLANT2,S_STAY+D_ROCK1+P_PLANT2
            .byte S_LEFT+L_RING,S_WIGG+P_PLANT2
            .byte S_STAY+D_ROCK1+P_PLANT2,S_STAY+D_ROCK2,S_RIGH ;+1
            .byte S_RIGH+D_ROCK1+L_RING,S_LEFT
            .byte S_STAY+D_ROCK2+P_PLANT2,S_RIGH+D_ROCK1,S_WIGG
            .byte S_STAY+D_SKEL+L_RING
            .byte S_STAY+D_ROCK2+L_RING,S_LEFT+D_ROCK1+P_PLANT2 ; 0
            .byte S_LEFT+D_ROCK1,S_LEFT+L_RING,S_STAY+D_ROCK2+P_PLANT2
            .byte S_RIGH+D_ROCK1,S_WIGG+P_PLANT2
            .byte S_STAY+D_ROCK2+L_RING,S_STAY,S_WIGG+D_ROCK1+L_LEVEL ; -1
            .byte S_RIGH+D_ROCK2,S_LEFT+D_ROCK1+L_RING,S_STAY,S_RIGH,S_WIGG
            .byte S_STAY+D_ROCK1,S_STAY+D_ROCK2+L_RING,S_LEFT ; 0
            .byte S_WIGG,S_LEFT+L_RING,S_STAY+D_ROCK2,S_RIGH+D_ROCK1,S_WIGG
            .byte S_STAY+D_ROCK2,S_STAY+L_RING,S_LEFT ; -1
            .byte S_RIGH+D_ROCK1,S_LEFT+L_RING,S_STAY+D_ROCK1
            .byte S_RIGH,S_WIGG+D_ROCK1+L_RING,S_STAY,S_STAY+D_ROCK2,S_LEFT ; 0
            
CavernLeft: .byte S_LEFT,S_LEFT+D_ROCK2,S_STAY,S_RIGH+D_ROCK1
            .byte S_WIGG+D_ROCK2,S_STAY+D_ROCK1+L_RING
            .byte S_STAY+D_ROCK2,S_WIGG+D_ROCK1 ; -1
            .byte S_RIGH+D_ROCK2+L_RING,S_LEFT,S_STAY,S_RIGH+D_ROCK1
            .byte S_WIGG+D_ROCK2,S_STAY+L_RING
            .byte S_STAY,S_LEFT+D_ROCK1+L_RING ; 0
            .byte S_WIGG+D_ROCK2+P_PLANT1,S_LEFT+D_ROCK1+L_RING,S_STAY,S_RIGH
            .byte S_WIGG+P_PLANT1,S_STAY+D_ROCK2,S_STAY,S_LEFT ; -1
            .byte S_RIGH+D_ROCK1,S_LEFT+D_ROCK2+L_RING,S_STAY
            .byte S_RIGH+D_ROCK2+L_RING,S_WIGG+D_ROCK1+P_PLANT1,S_STAY,S_STAY
            .byte S_LEFT+D_ROCK1 ; 0
            .byte S_RIGH+D_ROCK2+L_RING,S_RIGH,S_STAY+D_ROCK1
            .byte S_LEFT+D_ROCK2,S_WIGG+P_PLANT1
            .byte S_STAY,S_STAY+D_ROCK1+L_RING,S_WIGG ;+1
            .byte S_LEFT+D_ROCK2,S_RIGH+L_RING,S_STAY+D_ROCK2
            .byte S_LEFT+L_RING,S_WIGG+P_PLANT1
            .byte S_STAY,S_STAY+D_ROCK2+L_RING,S_RIGH ; 0
            .byte S_WIGG+P_PLANT1+D_ROCK2+L_RING
            .byte S_RIGH+D_ROCK1,S_STAY+D_ROCK2,S_LEFT
            .byte S_WIGG+P_PLANT1,S_STAY+D_ROCK2,S_STAY,S_RIGH+D_ROCK1 ;+1
            .byte S_RIGH,S_LEFT+D_ROCK1,S_STAY+D_ROCK2,S_RIGH
            .byte S_WIGG+D_ROCK1+P_PLANT1,S_STAY+L_RING
            .byte S_STAY+D_ROCK2,S_LEFT ; 0

            .byte S_WIGG+P_PLANT1,S_LEFT+D_ROCK1+L_RING,S_STAY,S_RIGH
            .byte S_WIGG+D_ROCK1,S_STAY,S_STAY+L_RING,S_LEFT ; -1
            .byte S_LEFT+D_ROCK2,S_RIGH+L_RING,S_STAY+D_ROCK1
            .byte S_LEFT,S_WIGG+P_PLANT1,S_STAY,S_STAY,S_RIGH ; 0
            .byte S_WIGG+P_PLANT1,S_RIGH+L_RING,S_STAY
            .byte S_LEFT,S_WIGG+D_ROCK2,S_STAY+L_RING,S_STAY,S_RIGH ;+1
            .byte S_RIGH+D_ROCK2,S_LEFT+L_RING,S_STAY+D_ROCK1
            .byte S_RIGH,S_WIGG+P_PLANT1,S_STAY+D_ROCK2
            .byte S_STAY,S_LEFT+D_ROCK1+L_RING ; 0
            .byte S_LEFT+D_ROCK2,S_LEFT+L_RING,S_STAY,S_RIGH+D_ROCK1
            .byte S_WIGG+D_ROCK2,S_STAY+L_RING
            .byte S_STAY,S_WIGG+D_ROCK1+P_PLANT1 ; -1
            .byte S_RIGH+D_ROCK1,S_LEFT+D_ROCK2,S_STAY+L_RING,S_RIGH
            .byte S_WIGG+P_PLANT1,S_STAY+D_ROCK1,S_STAY+L_RING
            .byte S_LEFT+D_ROCK1 ; 0
            .byte S_WIGG+P_PLANT1+D_ROCK2+L_RING
            .byte S_LEFT+D_ROCK1,S_STAY+D_ROCK2+L_RING,S_RIGH
            .byte S_WIGG+D_ROCK1+P_PLANT1,S_STAY+L_RING,S_STAY,S_LEFT ; -1
            .byte S_RIGH,S_LEFT+D_ROCK1+L_RING,S_STAY,S_RIGH
            .byte S_WIGG+D_ROCK1+P_PLANT1,S_STAY+D_ROCK2+L_RING
            .byte S_STAY,S_LEFT+D_ROCK2+L_RING ; 0
            
            .byte S_RIGH,S_RIGH+L_RING,S_STAY+D_ROCK2,S_LEFT
            .byte S_WIGG,S_STAY+L_RING,S_STAY,S_WIGG ;+1
            .byte S_LEFT,S_RIGH+P_PLANT1,S_STAY+L_RING
            .byte S_LEFT,S_WIGG,S_STAY,S_STAY+L_RING,S_RIGH ; 0
            .byte S_WIGG,S_RIGH+D_ROCK2+P_PLANT1,S_STAY,S_LEFT
            .byte S_WIGG,S_STAY,S_STAY+L_RING,S_RIGH ;+1
            .byte S_RIGH,S_LEFT+L_RING,S_STAY+P_PLANT1,S_RIGH
            .byte S_WIGG,S_STAY+P_PLANT1,S_STAY,S_LEFT ; 0
            .byte S_LEFT,S_LEFT+D_ROCK2+L_RING,S_STAY,S_RIGH
            .byte S_WIGG+P_PLANT1,S_STAY,S_STAY,S_WIGG ; -1
            .byte S_RIGH+D_SKEL,S_LEFT+P_PLANT1,S_STAY,S_RIGH
            .byte S_WIGG,S_STAY,S_STAY+L_RING,S_LEFT ; 0
            .byte S_WIGG+D_ROCK2,S_LEFT+P_PLANT1,S_STAY
            .byte S_RIGH,S_WIGG+P_PLANT1,S_STAY,S_STAY,S_LEFT ; -1
            .byte S_RIGH,S_LEFT+L_RING,S_STAY+D_ROCK2,S_RIGH
            .byte S_WIGG,S_STAY+P_PLANT1,S_STAY,S_LEFT ; 0
            
            .byte S_RIGH,S_RIGH+D_ROCK2+P_PLANT1,S_STAY
            .byte S_LEFT,S_WIGG+L_RING,S_STAY+P_PLANT1,S_STAY,S_WIGG ; +1
            .byte S_RIGH,S_LEFT,S_STAY+P_PLANT1
            .byte S_RIGH,S_WIGG,S_STAY+L_RING,S_STAY+P_PLANT1,S_LEFT ; 0
            .byte S_WIGG,S_LEFT+P_PLANT1,S_STAY+D_ROCK2
            .byte S_RIGH,S_WIGG+L_RING,S_STAY+P_PLANT1,S_STAY,S_LEFT ; -1
            .byte S_RIGH,S_LEFT,S_STAY+P_PLANT1,S_RIGH+L_RING
            .byte S_WIGG,S_STAY+P_PLANT1,S_STAY,S_LEFT+L_RING ; 0
            .byte S_RIGH,S_RIGH+D_SKEL,S_STAY+P_PLANT1,S_LEFT+D_ROCK2
            .byte S_WIGG,S_STAY+L_RING,S_STAY,S_WIGG ;+1
            .byte S_LEFT,S_RIGH+P_PLANT1,S_STAY+L_RING
            .byte S_LEFT,S_WIGG,S_STAY,S_STAY,S_RIGH+L_RING ; 0
            .byte S_WIGG+D_ROCK2+P_PLANT1
            .byte S_RIGH,S_STAY+L_RING,S_LEFT+P_PLANT1
            .byte S_WIGG,S_STAY+L_RING,S_STAY+L_RING,S_RIGH ;+1
            .byte S_RIGH,S_LEFT,S_STAY+L_RING,S_RIGH+P_PLANT1
            .byte S_WIGG,S_STAY+L_RING,S_STAY,S_LEFT ; 0

YouWonSt:   .byte (25+$80), (15+$80), (21+$80), (32+$80), (23+$80), (15+$80)
            .byte (14+$80), 0

GameOverSt: .byte (7+$80), (1+$80), (13+$80), (5+$80), (32+$80), (15+$80)
            .byte (22+$80), (5+$80), (18+$80), 0

MESSAGE:    .byte ('R'-'@'),('A'-'@'),('N'-'@'),('G'-'@'),('E'-'@'),('R'-'@'),0

DefChars:
            EMPTY = 0
            .byte %00000000     ; Blank char, ch. 5 (E)
            .byte %00000000
            .byte %00000000
            .byte %00000000
            .byte %00000000
            .byte %00000000
            .byte %00000000
            .byte %00000000

            WALL0A = 1
            .byte %00010000
            .byte %00100000
            .byte %01000000
            .byte %00100000
            .byte %00100000
            .byte %00010000
            .byte %00001000
            .byte %00010000

            WALL0B = 2
            .byte %00010000
            .byte %00001000
            .byte %00001000
            .byte %00010000
            .byte %00100000
            .byte %00010000
            .byte %00100000
            .byte %00010000

            WALL1A = 3
            .byte %00010000
            .byte %00100000
            .byte %00100000
            .byte %00010000
            .byte %00001000
            .byte %00000100
            .byte %00000010
            .byte %00000001

            WALL1B = 4
            .byte %10000000
            .byte %01000000
            .byte %01000000
            .byte %00100000
            .byte %00100000
            .byte %00010000
            .byte %00001000
            .byte %00010000

            WALL2A = 5
            .byte %00010000
            .byte %00001000
            .byte %00010000
            .byte %00010000
            .byte %00100000
            .byte %01000000
            .byte %01000000
            .byte %10000000

            WALL2B = 6
            .byte %00000001
            .byte %00000010
            .byte %00000100
            .byte %00000100
            .byte %00001000
            .byte %00010000
            .byte %00001000
            .byte %00010000

            SHIP = 7
            .byte %11000011
            .byte %11000011
            .byte %01111110
            .byte %00111100
            .byte %00011000
            .byte %01111110
            .byte %00011000
            .byte %00011000

            ROCK1 = 8
            .byte %00000000
            .byte %01111111
            .byte %00100100
            .byte %00100100
            .byte %11111110
            .byte %01001000
            .byte %01001000
            .byte %11111111

            ROCK2 = 9
            .byte %00000000
            .byte %00111000
            .byte %01000100
            .byte %10101010
            .byte %01010100
            .byte %00101000
            .byte %00010000
            .byte %00000000

            PLANT1 = 10
            .byte %10000000
            .byte %11000000
            .byte %11111100
            .byte %10101010
            .byte %00100101
            .byte %00100101
            .byte %00001001
            .byte %00010010

            PLANT2 = 11
            .byte %00000111
            .byte %01000100
            .byte %10111111
            .byte %10101010
            .byte %10100100
            .byte %00100100
            .byte %01001000
            .byte %00010000
            
            SKELETON1 = 12
            .byte %00011100
            .byte %00111110
            .byte %01101011
            .byte %01111111
            .byte %01010101
            .byte %00101010
            .byte %00011100
            .byte %00001000

            SKELETON2 = 13
            .byte %00111110
            .byte %01001001
            .byte %01011100
            .byte %01001000
            .byte %01011000
            .byte %10000000
            .byte %00000000
            .byte %00000000
            
            BORDER1L = 14
            .byte %11010010
            .byte %10101000
            .byte %11010101
            .byte %10100000
            .byte %11010100
            .byte %10100001
            .byte %01010100
            .byte %10101010
            
            BORDER2L = 15
            .byte %00100100
            .byte %10000001
            .byte %00001000
            .byte %00000010
            .byte %00100000
            .byte %00001000
            .byte %01000000
            .byte %00010000

            BORDER1R = 16
            .byte %01001011
            .byte %00010101
            .byte %10101011
            .byte %00000101
            .byte %00101011
            .byte %10000101
            .byte %00101010
            .byte %01010101
            
            BORDER2R = 17
            .byte %00100100
            .byte %10000001
            .byte %00010000
            .byte %01000000
            .byte %00000100
            .byte %00010000
            .byte %00000010
            .byte %00001000

            PLANTSHL = 18
            .byte %00000000
            .byte %11011000
            .byte %00000100
            .byte %11011110
            .byte %01111110
            .byte %00000100
            .byte %01101000
            .byte %00000000
            
            PLANTSHR = 19
            .byte %00000000
            .byte %00011010
            .byte %00100000
            .byte %01111101
            .byte %01111011
            .byte %00100000
            .byte %00011101
            .byte %00000000
            
            RING1 = 20
            .byte %00011000
            .byte %00100100
            .byte %01000010
            .byte %10000001
            .byte %10000001
            .byte %01000010
            .byte %00100100
            .byte %00011000
            
            RING2 = 21
            .byte %00011000
            .byte %00011000
            .byte %00100100
            .byte %01000010
            .byte %01000010
            .byte %00100100
            .byte %00011000
            .byte %00011000
        
            RING3 = 22
            .byte %00011000
            .byte %00011000
            .byte %00011000
            .byte %00100100
            .byte %00100100
            .byte %00011000
            .byte %00011000
            .byte %00011000
            
            RING4 = 23
            .byte %00011000
            .byte %00011000
            .byte %00011000
            .byte %00011000
            .byte %00011000
            .byte %00011000
            .byte %00011000
            .byte %00011000
            
            EXPLOSION1=24
            .byte %10000000     ; Block, ch. 11 (normally M)
            .byte %00100010
            .byte %10010000
            .byte %00110100
            .byte %11101110
            .byte %00110000
            .byte %00110011
            .byte %10000010
            
            GREMLIN = 25
            .byte %10000001
            .byte %01111110
            .byte %11011011
            .byte %11111111
            .byte %01100110
            .byte %00111100
            .byte %01000010
            .byte %10000001
            
            LASTCH = GREMLIN
            SPRITE1A = LASTCH+1
            SPRITE1B = LASTCH+2
            SPRITE1C = LASTCH+3
            SPRITE1D = LASTCH+4

            ; The ring character is changed during the game (it can be
            ; RING1, RING2, RING3 and RING4.
            
            RING     = LASTCH+5