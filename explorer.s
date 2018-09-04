;                V I C    C A V E R N   E X P L O R E R
;
;                   for the unexpanded Commodore VIC-20
;
;                 by Davide Bucci, August-September 2018
;
; Assembler: ca65

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
        tmpindex1 = $5      ; Temporary variables (byte) in DrawCavern

        str1      = $9      ; Address of the string to print with PrintStr (w.)
        tmp4      = $B      ; Temporary (b.)
        ColourRead= $C      ; Colour read by GetChar (b.)
        POSCHARPT = $D      ; Pointer for a character in memory (w.)
        POSCOLPT  = $F      ; Pointer for a colour in memory (w.)
        SPRITECH  = $11     ; Pointer to the group of 4 ch. for a sprite (word)
        CHRPTR    = $13     ; Pointer to the original ch. in a sprite (word)
        SpriteX   = $15     ; X position (offset in a char) of a sprite (byte)
        SpriteY   = $16     ; Y position (offset in a char) of a sprite (byte)
        CharShr   = $17     ; Employed in LoadSprite (b.)
        CharCode  = $18     ; Employed in DrawChar (b.)
        PosX      = $19     ; Positioning of the current char X (b.)
        PosY      = $1A     ; Positioning of the current char Y (b.)
        Colour    = $1B     ; Colour to be used by the printing routines (b.)
        CurrentCode=$1C     ; Code being processed for drawing walls (b.)
        Pos       = $1D     ; Used by PrintStr
        CavernPTR = $1E     ; Pointer to the cavern data (w.)
        TempOr    = $20     ; Temp. for PrepareCopy (b.)
        CurrentYPos=$21     ; Position in the cavern (b.)
        ShipYSpeed= $22     ; Vertical speed of ship (relative to scroll, b.)
        IrqCn     = $23     ; Counter for the IRQ
        Period    = $24     ; Period for the IRQ events
        CavernPosX= $25     ; While drawing cavern, the hor. shift in chrs (b.)
        VertPosPx = $26     ; Vertical shift position in pixels of the ship (b.)
        ShipPosX  = $27     ; X position of the ship in the game in pixels (b.)
        ShipPosY  = $28     ; Y position of the ship in the game in pixels (b.)
        Win       = $29     ; Specify if the game should be stopped (b.)
        VoiceBase = $2A     ; = 128 music on, = 0 music off (b.)
        keyin     = $2B     ; Last key typed.
        ShipChrX  = $2C     ; X position of the ship in characters (b.)
        ShipChrY  = $2D     ; Y position of the ship in characters (b.)
        ShipXSpeed= $2E     ; Horisontal speed of the ship (b.)
        CurrXPosL = $2F     ; Current shift (in ch.) of the left wall (b.)
        CurrXPosR = $30     ; Current shift (in ch.) of the right wall (b.)
        CurrCode  = $31     ; Current code while drawing the cavern (b.)
        SFXCounter= $32     ; Sound Effects counter (b.)
        Direction = $33     ; =0 if right border =128 if left border (b. cavern)
        SOURCE    = $34     ; Source pointer for CopyMem (w.)
        DEST      = $36     ; Destination pointer for CopyMem (w.)
        BLENDCHA  = $38     ; Saved ch. 1 to be blended with sprite 1 (b.)
        BLENDCHB  = $39     ; Saved ch. 2 to be blended with sprite 1 (b.)
        BLENDCHC  = $3A     ; Saved ch. 3 to be blended with sprite 1 (b.)
        BLENDCHD  = $3B     ; Saved ch. 4 to be blended with sprite 1 (b.)
        PlantShoot= $3C     ; Position of the plant shoot (b.)
        PlantDir  = $3D     ; Direction of the plant shoot (=0 or =128) (b.)
        TmpChar   = $3E     ; Temporary for DrawCharFast (b.)
        Score     = $3F     ; Current score (w.)
        HiScore   = $41     ; High score (w.)
        CanIncLev = $42     ; =0 if the level can be incremented =128 otherwise
        Level     = $43     ; Current level number (b.)
        UpdateWPos= $44     ; =0 if the wall position has to be shifted (b.)
        IRQfreec  = $45     ; Free running counter sync with IRQ (b.)
        TmpPos    = $46     ; Temporary for PosChar (b.)
        PlantPos  = $47     ; Temporary for PutPlant1/2 (b.)
        GremlinX  = $48     ; X position of the gremlin (b.)
        GremlinY  = $49     ; Y position of the gremlin (b.)

        ; Video settings (those things that depend between NTSC/PAL
        Vpos      = $50     ; Vertical position of the screen (top, b.)
        Vposm15   = $51     ; Same as Vpos but minus 15 :-)  (b.)
        Vposm16   = $52     ; Vposm15 minus one (b.)
        RasterlNMI= $53     ; Raster line for the NMI interrupt (b.)
        NormSCRpos= $54     ; Centered position of the screen (b.)
        MaxShipPos= $55     ; Maximum vertical position of the ship (b.)
        CavernSize= $56     ; Number of lines of the cavern (b.)


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
        PORTAVIA1  = $9111      ; Port A 6522 (joystick)
        ACRVIA1    = $911B
        PORTAVIA1d = $9113      ; Port A 6522 (joystick)
        T1CLVIA1   = $9114
        T1CHVIA1   = $9115
        T1LLVIA1   = $9116
        T1LHVIA1   = $9117
        IFRVIA1    = $911D      ; Interrupt flags
        IERVIA1    = $911E      ; Interrupt enable

        PORTBVIA2  = $9120      ; Port B 6522 2 value (joystick)
        PORTBVIA2d = $9122      ; Port B 6522 2 direction (joystick
        T1CLVIA2   = $9124
        T1CHVIA2   = $9125
        T1LLVIA2   = $9126
        T1LHVIA2   = $9127
        ACRVIA2    = $912B
        IFRVIA2    = $912D      ; Interrupt flags
        IERVIA2    = $912E      ; Interrupt enable

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
            cmp #60
            bpl @ok
            lda #60
            sta ShipPosY
            lda #0
            sta ShipYSpeed
@ok:        rts

CheckJoystick:
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
            rts

StartGame:  clc
            lda #$2F        ; Turn on the volume, set multicolour add. colour 2
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
            sta Score
            sta Score+1
            sta CanIncLev
            sta Level
            sta IRQfreec
            sta IrqCn
            lda #$80
            sta UpdateWPos
            sta GremlinX
            jsr CLSA
            ldx #4
            stx CurrXPosL
            ldx #11
            stx CurrXPosR
            lda #64
            sta ShipPosX
            sta ShipPosY
            lda Vpos       ; Put the screen again in the bottom position
            sta VICSCRVE
            jsr PutRings
            jsr ChangeRingState
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
            ldy #2              ; Starting at 2 allows to avoid negative values
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
@cont:      lda CurrCode
            and #%11111100      ; Shortcut!
            beq NoRings
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
            lda CurrCode        ; Handle plants, surface decorations, gremlins
            and #P_MASK
            beq NoSurface
            cmp #P_PLANT1
            bne @nd1
            jmp PutPlant1
@nd1:       cmp #P_PLANT2
            bne @nd3
            jmp PutPlant2
@nd3:       lda #7              ; Put a gremlin here
            sta GremlinX
            lda #31
            sta GremlinY
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
            clc                 ; The carry should be always clear here
            adc #4              ; Every step is 4 lines
            sta PosY
            cmp CavernSize      ; Check if we got to the end of the screen
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
            lda #RED
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

PutPlant1:  lda #PLANT1         ; Position the plant
            sta (POSCHARPT),Y
            lda #GREEN
            sta (POSCOLPT),Y
            sty PlantPos        ; Calculate the position of the shoot
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
            lda SFXCounter
            beq @exit
            lda EFFECTS
            ora VoiceBase
            sta EFFECTS
@exit:      jmp NoRings

PutPlant2:  lda #PLANT2         ; Position the plant
            sta (POSCHARPT),Y
            lda #GREEN
            sta (POSCOLPT),Y
            sty PlantPos        ; Calculate the position of the shoot
            dey
            tya
            sbc PlantShoot
            tay
            bit PlantDir        ; Check the direction of the plant's shoot
            bmi @right
            lda #PLANTSHR
            bne @cc             ; Branch always
@right:     lda #PLANTSHL
@cc:        sta (POSCHARPT),Y
            lda #YELLOW
            sta (POSCOLPT),Y
            ldy PlantPos
            lda SFXCounter      ; Put a little SFX on
            beq @exit
            lda EFFECTS
            ora VoiceBase
            sta EFFECTS
@exit:      jmp NoRings

; INIT - INIT - INIT - INIT - INIT - INIT - INIT - INIT - INIT - INIT - INIT
;
; Initialization code: prepare the screen to the correct size, center it and
; load the graphic chars and configure the IRQ handler.
;
; INIT - INIT - INIT - INIT - INIT - INIT - INIT - INIT - INIT - INIT - INIT

; Screen init value for PAL and NTSC

CenterScreenPAL:
            lda #30
            sta CavernSize
            lda #$22
            sta Vpos
            lda #155
            sta RasterlNMI
            lda #$3E        ; Set a 31 row-high column
            sta VICROWNC
            ldx #$12
            ldy #$16
            jmp ContInit

CenterScreenNTSC:
            lda #25
            sta CavernSize
            lda #22
            sta Vpos
            lda #130
            sta RasterlNMI
            lda #$36        ; Set a 27 row-high column
            sta VICROWNC
            ldx #$0A
            ldy #$10
            jmp ContInit

Init:       lda #$80        ; Autorepeat on on the keyboard
            sta REPEATKE
            sta VoiceBase
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
            lda INITVALC
            cmp #$05        ; Determine if we run on a PAL or NTSC machine
            beq CenterScreenNTSC    ; Load the screen settings
            bne CenterScreenPAL
ContInit:   sty VICSCRVE    ; Centre the screen vertically...
            stx VICSCRHO    ; ... and horizontally
            stx NormSCRpos
            lda Vpos
            sec
            sbc #15
            sta Vposm15
            sta Vposm16
            dec Vposm16
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
ContInit1:  sta T1LLVIA2
            stx T1CHVIA2    ; Set up the timer and start it
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
            sta IrqCn
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

; and NTSC

SyncNTSC:
            ; Data for NTSC machines. See for example:
            ; http://www.antimon.org/dl/c64/code/stable.txt
            LINES_NTSC = 261
            CYCLES_PER_LINE_NTSC = 65
            TIMER_VALUE_NTSC = LINES_NTSC * CYCLES_PER_LINE_NTSC - 2
@loopsync:  lda VICRAST     ; Synchronization loop
            cmp #110
            bne @loopsync
            lda #<TIMER_VALUE_NTSC
            ldx #>TIMER_VALUE_NTSC
            jmp ContInit1

; Synchronize the timer to the NMI interrupt to a given raster line.

SyncLater:
            lda RasterlNMI  ; Synchronization loop
@loopsync:  cmp VICRAST
            bne @loopsync
            rts



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

PrintRes:
            lda Res+2       ; Print all the BCD chars
            jsr PrintBCD
            lda Res+1
            jsr PrintBCD
            lda Res
            jmp PrintBCD

PutRings:
            ldy #255
@loop:      lda NMIHandler,y
            cmp #5
            bmi @noringr
            lda CavernRight,y
            ora #L_RING
            sta CavernRight,y
            bne @noringl        ; Branch always
@noringr:   lda NMIHandler,y
            cmp #20
            bmi @noringl
            lda CavernLeft,y
            ora #L_RING
            sta CavernLeft,y
@noringl:   dey
            bne @loop
            rts

; NMI - NMI - NMI - NMI - NMI - NMI - NMI - NMI - NMI - NMI - NMI - NMI - NMI
;
; This is the NMI handler, called 50 times each second when the VIC-20
; is a PAL unit or 60 when NTSC.
;
; NMI - NMI - NMI - NMI - NMI - NMI - NMI - NMI - NMI - NMI - NMI - NMI - NMI

NMIHandler: pha
            lda NormSCRpos
            sta VICSCRHO
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

; Implement some functions as macros, as they are called only once and they
; deserve to be inlined so to save jrs+rts (12 cycles, i.e. 12 µs)

.macro DrawGremlin
            ldy GremlinY
            beq @deactivate
            ldx GremlinX
            lda #BLUE
            sta Colour
            lda #GREMLIN
            jsr DrawChar
            jmp @skipm
@deactivate:
            lda #80
            sta GremlinX
@skipm:
.endmacro

.macro UpdatePlants
            bit PlantDir    ; Update the position of plant shoots
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
            bne @nochange
            lda #0
            sta PlantDir
@nochange:  asl
            asl
            sta EFFECTS
            lda #10
            sta SFXCounter
.endmacro

.macro UpdateGremlin
            ldx GremlinX
            lda GremlinY
            sec
            sbc #5
            sta GremlinY
            and #1
            bne @dec
            inc GremlinX
            beq @skip       ; Branch always
@dec:       dec GremlinX
@skip:
.endmacro

negspeed:   eor #$FF
            clc
            adc #1
            ;lda #0
            ;sec
            ;sbc ShipXSpeed
            jmp posspeed

stopgame:   jmp nodrawship

IrqHandler: pha
            txa             ; Save registers
            pha
            tya
            pha
            lda #09
            ;sta VICCOLOR
            lda #63         ; Switch "off" the screen, shifting all to right
            sta VICSCRHO
            lda ShipXSpeed
            bmi negspeed
posspeed:   asl
            asl
            asl
            asl
            ora VoiceBase
            sta VOICE1
            inc IRQfreec
            bit Win
            bmi stopgame
            lda SFXCounter  ; Check if we have a SFX on
            bne @FX         ; If no, shut off voice
            lda #0
            sta EFFECTS
            beq @normalcont ; branch always
@FX:        dec SFXCounter  ; If yes, decrement counter
@normalcont:dec IrqCn       ; Execute every PERIOD/60 of second
            beq @contint
            jmp @redraw
@contint:   lda Period      ; Restart the counter for the period
            sta IrqCn
            inc VertPosPx   ; Since the screen will be scrolled upwards,
            inc VertPosPx   ; compensate the position of the ship by 2 pixels
            inc ShipPosY    ; Increment the position of the ship (with respect
            bit ShipPosY    ; to the field) up to line 127. Fall down by gravity
            bpl @nopb       ; Check if we past 127th line.
            lda #127
            sta ShipPosY
@nopb:      dec VICSCRVE   ; Decrement the screen position so that everything
            lda VICSCRVE   ; scrolls towards the top by 2 raster lines.
            cmp Vposm16    ; 16 x 2 pixels = 4 lines
            bne @redraw
            lda Vpos       ; Put the screen again in the bottom position
            sta VICSCRVE
            inc CurrentYPos ; We increase the position in the cavern
            lda #0
            sta VertPosPx   ; We also put to 0 the shift in the ship position
            lda CanIncLev   ; And we decrement the counter for the dead time
            beq @greml      ; for incrementing the level
            dec CanIncLev
@greml:     bit GremlinX    ; See if the gremlin position has to be updated
            bmi @redraw
            UpdateGremlin   ; Inline a macro and save 12 cycles
@redraw:    lda VICSCRVE    ; Check if in the next iteration we should calculate
            cmp Vposm15     ; the shift of the cavern's walls.
            bne @chk
            lda IrqCn
            cmp #1
            bne @chk        ; Here we must prepare the shift in the walls for
            lda #$00        ; the next step.
            sta UpdateWPos
            UpdatePlants    ; Macro call that will be inlined
            jsr ChangeRingState
@chk:       jsr CLSA        ; Launch a complete redraw of the cavern
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
            ;clc             ; check if useful!!! (It is not!)
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
            tya
            sta Direction
            jsr DrawCavern
            lda CavernPosX
            sta CurrXPosR
            lda #128
            sta UpdateWPos
@endscroll:
            bit GremlinX
            bmi @nogremlin
            DrawGremlin
@nogremlin: jsr DrawShip
nodrawship: lda #$08
            sta VICCOLOR
            pla             ; Restore registers
            tay
            pla
            tax
            pla
            jmp $EABF       ; Jump to the standard IRQ handling routine

; Load the current shape of the ring in the generic character, following the
; value of PlantShoot (from 0 to 3).

ChangeRingState:
            lda PlantShoot
            and #$03
            adc #RING1
            tax
            ldy #RING
                        ; no rts here, as CopyChar is used afterwards

; Prepare for a copy of the memory of the character to be blended in the sprite
; 1 area.
; X = the caracter to be blended (source)
; Y = the sprite character (destination)
CopyChar:
            jsr PrepareCopy
CopyMem:    lda (SOURCE),y
            sta (DEST),y
            dey
            bne CopyMem
            lda (SOURCE),y
            sta (DEST),y
            rts

CheckCrash:
            ldx ShipChrX
            ldy ShipChrY
            jsr PosChar
            ldy #0
            lda (POSCHARPT),y
            cmp #SPRITE1A
            beq @skip
            sta BLENDCHA
            iny
            lda (POSCHARPT),y
            sta BLENDCHC
            ldy #16
            lda (POSCHARPT),y
            sta BLENDCHB
            iny
            lda (POSCHARPT),y
            sta BLENDCHD
@skip:
            ldx BLENDCHA        ; The code 0 is for the EMPTY char
            beq @next1
            cpx #SPRITE1A
            beq @next4
            ldy #SPRITE1A
            jsr CheckCollision
@next1:     ldx BLENDCHB
            beq @next2
            ldy #SPRITE1B
            jsr CheckCollision
@next2:     ldx BLENDCHC
            beq @next3
            ldy #SPRITE1C
            jsr CheckCollision
@next3:     ldx BLENDCHD
            beq @next4
            ldy #SPRITE1D
            jmp CheckCollision
@next4:     rts

IncrementLevel:
            lda #('L'-'@')  ; Show L followed by the number of the level
            sta (POSCHARPT),Y
            lda Level
            sec
            adc #(48+$80) ; Convert into a number
            iny
            sta (POSCHARPT),Y
            lda #WHITE
            sta (POSCOLPT),Y
            dey
            bit Direction
            bmi @normal
            lda CanIncLev
            bne @normal
            lda #10
            sta CanIncLev
            lda Level
            cmp #LEVELNO-1
            beq @maxlev
            inc Level
@maxlev:    txa
            pha
            ldx Level
            lda LevelSpeed,X
            sta Period
            pla
            tax
@normal:    clc
            jmp NoRings



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
            lda tmp4
            lsr
            lsr
            lsr
            clc
            adc #1          ; The line that correspond to highest ship pos.
            tay             ; X and Y now contain the new positions of the ship
            sty ShipChrY
            stx ShipChrX
NormalShip: lda #SHIP
            sta CharCode
            lda #MAGENTA
            sta Colour
            jsr LoadAppropriateSprite
            jsr CheckCrash
            jsr BlendSprite
            jmp DrawSprite

; Draw the current sprite at the ShipChrX, ShipChrY position

DrawSprite: ldx ShipChrX
            ldy ShipChrY
            jsr PosChar
            ldy #0
            lda #SPRITE1A
            sta (POSCHARPT),Y
            lda Colour
            sta (POSCOLPT),Y
            iny
            lda #SPRITE1C
            sta (POSCHARPT),Y
            lda Colour
            sta (POSCOLPT),Y
            lda #SPRITE1B
            ldy #16
            sta (POSCHARPT),Y
            lda Colour
            sta (POSCOLPT),Y
            iny
            lda #SPRITE1D
            sta (POSCHARPT),Y
            lda Colour
            sta (POSCOLPT),Y
            rts

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
                            ; Here we collected a ring.
            lda ShipChrY
            lsr             ; Divide by four
            lsr
            clc
            adc CurrentYPos
            tay
            lda CavernLeft,y
            and #%00111111
            sta CavernLeft,y
            lda CavernRight,y
            and #%00111111
            sta CavernRight,y
            lda #10
            sta SFXCounter
            lda #110
            ora VoiceBase
            sta EFFECTS
            lda #1
            jmp AddScore


; Stop the game, explode the ship and die :-(
Die:        bit Win         ; The routine can be called twice if the collision
            bpl @first      ; is done in multiple characters of a sprite.
            rts             ; Exit if it is the second call!
@first:     lda #$FF
            sta Win         ; Stop the game
            sta VICCOLOR    ; Light yellow screen, yellow border
            lda #192
            sta NOISE
            lda #EXPLOSION1
            sta CharCode
            lda #MULTICOLOUR
            sta Colour
            jsr LoadAppropriateSprite
            jsr DrawSprite
            jsr ShortDelay
            lda #$28
            sta VOLUME
            lda #125        ; Yellow screen, green border
            sta VICCOLOR
            jsr ShortDelay
            lda #$24
            sta VOLUME
            lda #42         ; Red screen, red border
            sta VICCOLOR
            jsr ShortDelay
            lda #$A         ; Black screen, red border
            sta NOISE
            sta VICCOLOR
            jsr ShortDelay
            lda #$20
            sta VOLUME
            ldy CurrentYPos ; Put a little skeleton in the cavern
            iny
            iny
            iny
            iny
            iny
            iny
            lda (CavernPTR),y
            ora #D_SKEL
            sta (CavernPTR),y
            LDA Score+1  ; compare high bytes
            CMP HiScore+1
            BCC @label2 ; if NUM1H < NUM2H then NUM1 < NUM2
            BNE @label1 ; if NUM1H <> NUM2H then NUM1 > NUM2 (so NUM1 >= NUM2)
            LDA Score  ; compare low bytes
            CMP HiScore
            BCC @label2 ; if NUM1L < NUM2L then NUM1 < NUM2
@label1:    lda Score+1
            sta HiScore+1
            lda Score
            sta HiScore
@label2:
            ldx #6
            ldy #7
            lda #YELLOW
            sta Colour
            lda #<ScoreMSG
            sta str1
            lda #>ScoreMSG
            sta str1+1
            jsr PrintStr
            lda Score       ; Load the current score and convert it to BCD
            sta Val
            lda Score+1
            sta Val+1
            jsr Bin2BCD
            ldx #5
            iny
            jsr PrintRes
            ldx #5
            iny
            lda #CYAN
            sta Colour
            lda #<HiScoreMSG
            sta str1
            lda #>HiScoreMSG
            sta str1+1
            jsr PrintStr
            lda HiScore       ; Load the current score and convert it to BCD
            sta Val
            lda HiScore+1
            sta Val+1
            jsr Bin2BCD
            ldx #5
            iny
            jmp PrintRes

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
            sta TmpChar
            jsr PosChar
            ldy #0
            lda Colour
            sta (POSCOLPT),Y
            lda TmpChar
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
            cpy #15          ; 2 cycles
            tya
            asl
            asl
            asl
            asl
            bcc @nocorr
            inc POSCHARPT+1
            clc
@nocorr:    
            adc PosX
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
            asl
            asl
            asl
            asl
            bcc @nocorr
            inc POSCHARPT+1
            inc POSCOLPT+1
            clc
@nocorr:    adc PosX
            sta TmpPos
            adc POSCHARPT
            sta POSCHARPT
            lda TmpPos
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
            sta TmpPos
            adc #16
            tay
            lda CharShr
            ora (SPRITECH),y
            sta (SPRITECH),y    ; Save
            lda TmpPos
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
            dey
            sta (SPRITECH),y    ; sta does not affect processor flags
            dey
            sta (SPRITECH),y    ; sta does not affect processor flags
            dey
            sta (SPRITECH),y    ; sta does not affect processor flags
            bne @loop
            rts

; Blend the sprite characters with the four characters that are on the
; background.

BlendSprite:
            ldx BLENDCHA        ; Code 0 is the EMPTY char
            beq @skip1
            ldy #SPRITE1A
            jsr OrChar
@skip1:     ldx BLENDCHB
            beq @skip2
            ldy #SPRITE1B
            jsr OrChar
@skip2:     ldx BLENDCHC
            beq @skip3
            ldy #SPRITE1C
            jsr OrChar
@skip3:     ldx BLENDCHD
            beq skip4
            ldy #SPRITE1D

            ; no rts here

; Blend two characters by putting in the destination the OR between the source
; and the destination

OrChar:     jsr PrepareCopy
            ;                   
OrMem:      lda (SOURCE),y
            ora (DEST),y
            sta (DEST),y
            dey
            bne OrMem
            lda (SOURCE),y
            ora (DEST),y
            sta (DEST),y
skip4:      rts

PrepareCopy:
            stx CharCode
            sty TempOr
            jsr CalcChGenOfs
            lda CHRPTR
            sta SOURCE
            lda CHRPTR+1
            sta SOURCE+1
            lda TempOr
            sta CharCode
            jsr CalcChGenOfs
            lda CHRPTR
            sta DEST
            lda CHRPTR+1
            sta DEST+1
            ldy #7
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


; Clear the screen.

CLSA:
            lda #EMPTY
            ; no return here!

; Fill the screen.
; Draw everywhere the character contained in the A register. Employs X.

CLS:
            size=16*31/16+1
            ldx #size
@loop:      sta MEMSCR-1,X          ; A degree of loop unrolling avoids
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

; A simple delay

ShortDelay: ldy #$40
            ldx #$FF
Delayloop:  dex
            bne Delayloop
            dey
            bne Delayloop
            rts

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

S_MASK = %00000011  ; Displacement (positive: go right, negative: go left)
S_RIGH = %00000001  ; +1
S_LEFT = %00000010  ; -1
S_STAY = %00000000  ; 0
S_WIGG = %00000011  ; 0

D_MASK  = %00001100
D_ROCK1 = %00000100
D_ROCK2 = %00001000
D_SKEL  = %00001100 ; Skeleton code should be 11 as it can be OR'ed

P_MASK   = %00110000
P_PLANT1 = %00010000
P_PLANT2 = %00100000
P_GREML  = %00110000

L_MASK = %11000000
L_RING = %01000000
L_LEVEL= %10000000

CAVERNLENR = 255

; For each cavern wall, there should be exactly as much as S_RIGH that S_LEFT,
; so that the size of position of the wall will roll back to the original
; one at the beginning of the cavern. The cavern can thus be passed through
; an infinite number of times and will not be 'crunching' or 'expanding'.

CavernRight:
            .byte S_RIGH,S_RIGH,S_STAY,S_LEFT,S_WIGG
            .byte S_STAY,S_STAY,S_WIGG ;+1  (1)
            .byte S_LEFT,S_RIGH,S_STAY
            .byte S_LEFT,S_WIGG+P_PLANT2
            .byte S_STAY,S_STAY,S_RIGH ; 0   (2)
            .byte S_WIGG,S_RIGH+D_ROCK1,S_STAY,S_LEFT
            .byte S_WIGG,S_STAY+D_ROCK1,S_STAY,S_RIGH ;+1   (3)
            .byte S_RIGH+D_ROCK1,S_LEFT+P_PLANT2,S_STAY,S_RIGH+D_ROCK2
            .byte S_WIGG,S_STAY+D_ROCK1
            .byte S_STAY+D_ROCK1,S_LEFT+L_LEVEL ; 0 (4) ***** LEVEL2 ******
            .byte S_LEFT,S_LEFT+D_ROCK1,S_STAY,S_RIGH+D_ROCK1
            .byte S_WIGG,S_STAY+D_ROCK1,S_STAY
            .byte S_WIGG+P_PLANT2 ; -1 (5)
            .byte S_RIGH,S_LEFT+D_ROCK1,S_STAY,S_RIGH
            .byte S_WIGG+P_PLANT2+D_ROCK1
            .byte S_STAY,S_STAY,S_LEFT+P_PLANT2 ; 0 (6)
            .byte S_WIGG+D_ROCK1,S_LEFT,S_STAY+D_ROCK1
            .byte S_RIGH+D_ROCK2
            .byte S_WIGG,S_STAY+D_ROCK1,S_STAY,S_LEFT+P_PLANT2 ; -1 (7)
            .byte S_RIGH+D_ROCK1,S_LEFT,S_STAY+D_ROCK1
            .byte S_RIGH,S_WIGG+P_PLANT2
            .byte S_STAY+D_ROCK1,S_STAY,S_LEFT ; 0 (8) net: 0

            .byte S_WIGG+D_ROCK1,S_RIGH+D_ROCK2,S_STAY+D_ROCK1,S_LEFT
            .byte S_WIGG+P_PLANT2+D_ROCK1
            .byte S_LEFT+D_ROCK1
            .byte S_LEFT+D_ROCK2,S_RIGH+L_LEVEL ; -1 (9) ***** LEVEL3 ******
            .byte S_RIGH+D_ROCK2+P_GREML,S_LEFT+D_ROCK1,S_STAY
            .byte S_RIGH+D_ROCK1,S_WIGG
            .byte S_STAY,S_STAY+D_ROCK1,S_LEFT ; 0 (10)
            .byte S_WIGG+D_ROCK1+P_GREML,S_LEFT+D_ROCK2,S_STAY,S_RIGH+D_ROCK2
            .byte S_WIGG+P_PLANT2+D_ROCK1
            .byte S_STAY+D_ROCK1,S_STAY,S_LEFT+D_ROCK2 ; -1 (11)
            .byte S_RIGH+D_ROCK2,S_LEFT+D_ROCK1,S_STAY+D_ROCK1
            .byte S_RIGH,S_WIGG+D_ROCK1,S_STAY,S_STAY+D_ROCK2
            .byte S_LEFT+D_ROCK1+P_GREML ; 0 (12)
            .byte S_RIGH,S_RIGH+D_ROCK2,S_STAY+D_ROCK1,S_LEFT
            .byte S_WIGG,S_STAY+D_ROCK2
            .byte S_STAY,S_WIGG+P_GREML ;+1 (13)
            .byte S_LEFT+D_ROCK1,S_RIGH,S_STAY+D_ROCK2
            .byte S_LEFT+D_ROCK1,S_WIGG+P_PLANT2
            .byte S_STAY+D_ROCK1,S_STAY+D_ROCK1,S_RIGH+P_GREML ; 0 (14)
            .byte S_WIGG,S_RIGH+D_ROCK1,S_STAY,S_LEFT,S_WIGG
            .byte S_STAY+D_ROCK2,S_STAY,S_RIGH+D_ROCK2 ;+1 (15)
            .byte S_RIGH+D_ROCK1,S_LEFT+P_PLANT2+D_ROCK2,S_STAY
            .byte S_RIGH,S_WIGG,S_STAY+P_GREML
            .byte S_STAY+D_ROCK2
            .byte S_STAY+D_ROCK1+L_LEVEL+P_GREML ; +1 (16) ***** LEVEL4 ******
            ; tot: +1

            .byte S_LEFT+D_ROCK1,S_LEFT,S_STAY+D_ROCK1,S_RIGH+D_ROCK2
            .byte S_WIGG+P_PLANT2+D_ROCK1,S_STAY
            .byte S_STAY,S_WIGG+P_PLANT2+D_ROCK2 ; -1 (17)
            .byte S_RIGH+D_ROCK2,S_LEFT+D_ROCK1,S_STAY,S_RIGH+D_ROCK2
            .byte S_WIGG+D_ROCK2
            .byte S_STAY+D_ROCK2,S_STAY+D_ROCK1,S_LEFT+D_ROCK2 ; 0 (18)
            .byte S_WIGG+D_ROCK1,S_LEFT+D_ROCK2,S_STAY,S_RIGH+D_ROCK2
            .byte S_WIGG+P_PLANT2
            .byte S_STAY+D_ROCK2,S_STAY+D_ROCK1,S_LEFT ; -1 (19)
            .byte S_RIGH+D_ROCK1,S_LEFT,S_STAY,S_RIGH+D_ROCK2
            .byte S_WIGG+P_PLANT2
            .byte S_STAY+D_ROCK1,S_STAY
            .byte S_LEFT+D_ROCK2+L_LEVEL ; 0 (20) ***** LEVEL5 ******
            .byte S_RIGH+D_ROCK2,S_RIGH+D_ROCK1,S_STAY
            .byte S_LEFT,S_WIGG+D_ROCK1,S_STAY+D_ROCK1
            .byte S_STAY+D_ROCK2,S_WIGG ;+1 (21)
            .byte S_LEFT,S_RIGH+D_ROCK1,S_STAY,S_LEFT+D_ROCK2
            .byte S_WIGG+P_PLANT2
            .byte S_STAY+D_ROCK1,S_STAY+D_ROCK2,S_RIGH+D_ROCK1 ; 0 (22)
            .byte S_WIGG+D_ROCK2,S_RIGH
            .byte S_STAY+D_ROCK1,S_LEFT+P_PLANT2+D_ROCK1
            .byte S_WIGG+D_ROCK1,S_STAY+D_ROCK1
            .byte S_STAY,S_RIGH+D_ROCK2 ;+1 (23)
            .byte S_RIGH,S_LEFT+D_ROCK1,S_STAY+D_ROCK1
            .byte S_RIGH+P_PLANT2+D_ROCK2
            .byte S_WIGG+P_PLANT2,S_STAY+D_ROCK2
            .byte S_STAY+D_ROCK1
            .byte S_LEFT+D_ROCK2+L_LEVEL ; 0 (24) ***** LEVEL6 ****** tot: 0

            .byte S_WIGG+D_ROCK1,S_LEFT+D_ROCK2+P_PLANT2,S_STAY
            .byte S_RIGH+D_ROCK1,S_WIGG
            .byte S_STAY+D_ROCK1,S_STAY+D_ROCK2+P_PLANT2
            .byte S_LEFT ; -1 (25)
            .byte S_LEFT+D_ROCK1+P_PLANT2,S_RIGH,S_STAY
            .byte S_LEFT,S_WIGG+D_ROCK1
            .byte S_STAY+P_PLANT2,S_STAY+D_ROCK2+P_PLANT2,S_RIGH ; 0 (26)
            .byte S_WIGG+D_ROCK2,S_RIGH+P_PLANT2,S_STAY+D_ROCK1+P_PLANT2
            .byte S_LEFT,S_WIGG+P_PLANT2
            .byte S_STAY+D_ROCK1+P_PLANT2,S_STAY+D_ROCK2,S_RIGH ;+1 (27)
            .byte S_RIGH+D_ROCK1,S_LEFT
            .byte S_STAY+D_ROCK2+P_PLANT2,S_RIGH+D_ROCK1,S_WIGG
            .byte S_STAY
            .byte S_STAY+D_ROCK2
            .byte S_LEFT+D_ROCK1+P_PLANT2+L_LEVEL ; 0 (28) ***** LEVEL7 ******
            .byte S_LEFT+D_ROCK1,S_LEFT,S_STAY+D_ROCK2+P_PLANT2
            .byte S_RIGH+D_ROCK1,S_WIGG+P_PLANT2
            .byte S_STAY+D_ROCK2,S_STAY,S_WIGG+D_ROCK1+L_LEVEL ; -1 (29)
            .byte S_RIGH+D_ROCK2,S_LEFT+D_ROCK1,S_STAY,S_RIGH,S_WIGG
            .byte S_STAY+D_ROCK1,S_STAY+D_ROCK2,S_LEFT ; 0 (30)
            .byte S_WIGG,S_LEFT,S_STAY+D_ROCK2,S_RIGH+D_ROCK1,S_WIGG
            .byte S_STAY+D_ROCK2,S_STAY,S_LEFT ; -1 (31)
            .byte S_RIGH+D_ROCK1,S_LEFT,S_STAY+D_ROCK1
            .byte S_RIGH,S_RIGH+D_ROCK1
            .byte S_STAY,S_STAY+D_ROCK2,S_LEFT ; 0 (32) tot: -1

CavernLeft:
            .byte S_LEFT,S_LEFT,S_STAY,S_RIGH
            .byte S_WIGG,S_STAY
            .byte S_STAY,S_WIGG ; -1 (1)
            .byte S_RIGH,S_LEFT,S_STAY,S_RIGH
            .byte S_WIGG,S_STAY
            .byte S_STAY,S_LEFT+D_ROCK1 ; 0 (2)
            .byte S_WIGG+D_ROCK2+P_PLANT1,S_LEFT+D_ROCK1,S_STAY,S_RIGH
            .byte S_WIGG+P_PLANT1,S_STAY+D_ROCK2,S_STAY,S_LEFT ; -1 (3)
            .byte S_RIGH+D_ROCK1,S_LEFT+D_ROCK2,S_STAY
            .byte S_RIGH+D_ROCK2,S_WIGG+D_ROCK1+P_PLANT1,S_STAY,S_STAY
            .byte S_LEFT+D_ROCK1++L_LEVEL ; 0 (4)  ***** LEVEL2 ******
            .byte S_RIGH+D_ROCK2,S_RIGH,S_STAY+D_ROCK1
            .byte S_LEFT+D_ROCK2,S_WIGG+P_PLANT1
            .byte S_STAY,S_STAY+D_ROCK1,S_WIGG ;+1 (5)
            .byte S_LEFT+D_ROCK2,S_RIGH,S_STAY+D_ROCK2
            .byte S_LEFT,S_WIGG+P_PLANT1
            .byte S_STAY,S_STAY+D_ROCK2,S_RIGH ; 0 (6)
            .byte S_WIGG+P_PLANT1+D_ROCK2
            .byte S_RIGH+D_ROCK1,S_STAY+D_ROCK2,S_LEFT
            .byte S_WIGG+P_PLANT1,S_STAY+D_ROCK2,S_STAY,S_RIGH+D_ROCK1 ;+1 (7)
            .byte S_RIGH,S_LEFT+D_ROCK1,S_STAY+D_ROCK2,S_RIGH
            .byte S_WIGG+D_ROCK1+P_PLANT1,S_STAY
            .byte S_STAY+D_ROCK2,S_LEFT ; 0 (8) tot: 0

            .byte S_WIGG+P_PLANT1,S_LEFT+D_ROCK1,S_STAY,S_RIGH
            .byte S_WIGG+D_ROCK1,S_STAY
            .byte S_STAY,S_LEFT+L_LEVEL ; -1 (9) ***** LEVEL3 ******
            .byte S_LEFT+D_ROCK2,S_RIGH,S_STAY+D_ROCK1
            .byte S_LEFT,S_WIGG+P_PLANT1,S_STAY,S_STAY,S_RIGH ; 0 (10)
            .byte S_WIGG+P_PLANT1,S_RIGH,S_STAY
            .byte S_LEFT,S_WIGG+D_ROCK2,S_STAY,S_STAY,S_RIGH ;+1 (11)
            .byte S_RIGH+D_ROCK2,S_LEFT,S_STAY+D_ROCK1
            .byte S_RIGH,S_WIGG+P_PLANT1,S_STAY+D_ROCK2
            .byte S_STAY,S_LEFT+D_ROCK1 ; 0 (12)
            .byte S_LEFT+D_ROCK2,S_LEFT,S_STAY,S_RIGH+D_ROCK1
            .byte S_WIGG+D_ROCK2,S_STAY
            .byte S_STAY,S_WIGG+D_ROCK1+P_PLANT1 ; -1 (13)
            .byte S_RIGH+D_ROCK1,S_LEFT+D_ROCK2,S_STAY,S_RIGH
            .byte S_WIGG+P_PLANT1,S_STAY+D_ROCK1,S_STAY
            .byte S_LEFT+D_ROCK1 ; 0 (14)
            .byte S_WIGG+P_PLANT1+D_ROCK2
            .byte S_LEFT+D_ROCK1,S_STAY+D_ROCK2,S_RIGH
            .byte S_WIGG+D_ROCK1+P_PLANT1,S_STAY,S_STAY,S_LEFT ; -1 (15)
            .byte S_RIGH,S_LEFT+D_ROCK1,S_STAY,S_RIGH
            .byte S_WIGG+D_ROCK1+P_PLANT1,S_STAY+D_ROCK2,S_STAY
            .byte S_LEFT+D_ROCK2+L_LEVEL ; 0 (16) ***** LEVEL4 ******
            ; tot: -2

            .byte S_RIGH,S_RIGH,S_STAY+D_ROCK2,S_LEFT
            .byte S_WIGG,S_STAY,S_STAY,S_WIGG ;+1 (17)
            .byte S_LEFT,S_RIGH+P_PLANT1,S_STAY
            .byte S_LEFT,S_WIGG,S_STAY,S_STAY,S_RIGH ; 0 (18)
            .byte S_WIGG,S_RIGH+D_ROCK2+P_PLANT1,S_STAY,S_LEFT
            .byte S_WIGG,S_STAY,S_STAY,S_RIGH ;+1 (19)
            .byte S_RIGH,S_LEFT,S_STAY+P_PLANT1,S_RIGH
            .byte S_WIGG,S_STAY+P_PLANT1
            .byte S_STAY,S_LEFT+L_LEVEL ; 0 (20) ***** LEVEL5 ******
            .byte S_LEFT,S_LEFT+D_ROCK2,S_STAY,S_RIGH
            .byte S_WIGG+P_PLANT1,S_STAY,S_STAY,S_WIGG ; -1 (21)
            .byte S_RIGH,S_LEFT+P_PLANT1,S_STAY,S_RIGH
            .byte S_WIGG,S_STAY,S_STAY,S_LEFT ; 0 (22)
            .byte S_WIGG+D_ROCK2,S_LEFT+P_PLANT1,S_STAY
            .byte S_RIGH,S_WIGG+P_PLANT1,S_STAY,S_STAY,S_LEFT ; -1 (23)
            .byte S_RIGH,S_LEFT,S_STAY+D_ROCK2,S_RIGH
            .byte S_WIGG,S_STAY+P_PLANT1
            .byte S_STAY,S_LEFT+L_LEVEL ; 0 (24) ***** LEVEL6 ******
            ; tot: 0

            .byte S_RIGH,S_RIGH+D_ROCK2+P_PLANT1,S_STAY
            .byte S_LEFT,S_WIGG,S_STAY+P_PLANT1
            .byte S_STAY,S_WIGG ; +1 (25)
            .byte S_RIGH,S_LEFT,S_STAY+P_PLANT1
            .byte S_RIGH,S_WIGG,S_STAY,S_STAY+P_PLANT1,S_LEFT ; 0 (26)
            .byte S_WIGG,S_LEFT+P_PLANT1,S_STAY+D_ROCK2
            .byte S_RIGH,S_WIGG,S_STAY+P_PLANT1,S_STAY,S_LEFT ; -1 (27)
            .byte S_RIGH,S_LEFT,S_STAY+P_PLANT1,S_RIGH
            .byte S_WIGG,S_STAY+P_PLANT1,S_STAY
            .byte S_LEFT+L_LEVEL ; 0 (28) ***** LEVEL7 ******
            .byte S_RIGH,S_RIGH,S_STAY+P_PLANT1,S_LEFT+D_ROCK2
            .byte S_WIGG,S_STAY,S_STAY,S_WIGG ;+1 (29)
            .byte S_LEFT,S_RIGH+P_PLANT1,S_STAY
            .byte S_LEFT,S_WIGG,S_STAY,S_STAY,S_RIGH ; 0 (30)
            .byte S_WIGG+D_ROCK2+P_PLANT1
            .byte S_RIGH,S_STAY,S_LEFT+P_PLANT1
            .byte S_WIGG,S_STAY,S_STAY,S_RIGH ;+1 (31)
            .byte S_RIGH,S_LEFT,S_STAY,S_RIGH+P_PLANT1
            .byte S_WIGG,S_STAY,S_STAY,S_LEFT ; 0 (32)
            ; tot: +2

HiScoreMSG: .byte ('H'-'@'),('I'-'@')
ScoreMSG:
            .byte ('S'-'@'),('C'-'@'),('O'-'@'),('R'-'@'),('E'-'@')
            .byte 0

; Speed of the levels and description

LEVELNO = 9
LevelSpeed: .byte 4             ; L1: Slow speed, easy level
            .byte 3             ; L2: L1 + More plants, more speed
            .byte 3             ; L3: L2 + Gremlins
            .byte 3             ; L4: L3 + Smaller cavern
            .byte 2             ; L5: L4 + More speed
            .byte 2             ; L6:
            .byte 1             ; L7: Maximum speed, large cavern
            .byte 1             ; L8: As L2, maximum speed
            .byte 1             ; L9: As L3, maximum speed

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
            .byte %00011000
            .byte %10000001
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
            .byte %00000001
            .byte %00000010
            .byte %00011100
            .byte %01100010
            .byte %01000101
            .byte %01000101
            .byte %00100010
            .byte %00010000

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

            PLANTSHL = 14
            .byte %00000000
            .byte %11011000
            .byte %00000100
            .byte %11011110
            .byte %01111110
            .byte %00000100
            .byte %01101000
            .byte %00000000

            PLANTSHR = 15
            .byte %00000000
            .byte %00011010
            .byte %00100000
            .byte %01111101
            .byte %01111011
            .byte %00100000
            .byte %00011101
            .byte %00000000

            RING1 = 16
            .byte %00011000
            .byte %00100100
            .byte %01000010
            .byte %10000001
            .byte %10000001
            .byte %01000010
            .byte %00100100
            .byte %00011000

            RING2 = 17
            .byte %00011000
            .byte %00011000
            .byte %00100100
            .byte %01000010
            .byte %01000010
            .byte %00100100
            .byte %00011000
            .byte %00011000

            RING3 = 18
            .byte %00011000
            .byte %00011000
            .byte %00011000
            .byte %00100100
            .byte %00100100
            .byte %00011000
            .byte %00011000
            .byte %00011000

            RING4 = 19
            .byte %00011000
            .byte %00011000
            .byte %00011000
            .byte %00011000
            .byte %00011000
            .byte %00011000
            .byte %00011000
            .byte %00011000

            EXPLOSION1=20
            .byte %10000000 
            .byte %00100010
            .byte %10010000
            .byte %00110100
            .byte %11101110
            .byte %00110000
            .byte %00110011
            .byte %10000010

            GREMLIN = 21
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