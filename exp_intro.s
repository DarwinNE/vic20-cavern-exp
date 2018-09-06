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

        INITVALC=$ede4

; KERNAL routines used
        GETIN = $FFE4

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
        
        secs = $7           ; Write in the second half of the screen

        str1      = $9      ; Address of the string to print with PrintStr (w.)
        tmp4      = $B      ; Temporary (b.)
        portaconfig=$C

        CharCode  = $18     ; Employed in DrawChar (b.)

        PosX      = $19     ; Positioning of the current char X (b.)
        PosY      = $1A     ; Positioning of the current char Y (b.)
        Colour    = $1B     ; Colour to be used by the printing routines (b.)
        RasterlNMI= $53     ; Raster line for the NMI interrupt (b.)

.export main
.segment "STARTUP"
.segment "LOWCODE"
.segment "INIT"
.segment "GRCHARS"
.segment "CODE"

main:
            jsr Init
@loopchar:  lda PORTAVIA1   ; Load joystick state
            and #32         ; Check for the fire button
            cmp #32
            bne @doload
            jsr GETIN       ; Main loop waiting for keyboard events
            beq @loopchar
@doload:    lda portaconfig ; Put back configuration for port A of VIA 1
            sta PORTAVIA1d  ; to avoid loading issues
            lda #$00
            sta GEN1
            sta GEN2
            sta GEN3
            sta VOLUME
            sta 646         ; Change current colour to black
            sei             ; Configure the interrupt handler
            lda #<$EABF
            sta $0314
            lda #>$EABF
            sta $0315
            cli
            lda #8
            sta 36879
            lda #240
            sta 36869

            lda #240
            sta 36869
            jsr $E55F           ; Clear the screen and home cursor
            ldx #21
            ldy #$0
            lda #$0
            stx secs
            lda #<LoadStr       ; Write a load command
            sta str1
            lda #>LoadStr
            sta str1+1
            lda #BLACK
            sta Colour
            jsr PrintStr
            lda 186             ; Get the last opened device
            clc
            cmp #01
            beq @cassette
            cmp #08
            beq @disk8
            cmp #09
            beq @disk9
            cmp #10
            beq @disk10
            cmp #11
            beq @disk11

@loadc:     jsr DrawChar        ; Complete the load command
            inx
            lda #<EndL
            sta str1
            lda #>EndL
            sta str1+1
            lda #BLACK
            sta Colour
            jsr PrintStr
            lda #<Loading
            sta str1
            lda #>Loading
            sta str1+1
            lda #WHITE
            sta Colour
            lda #01
            sta secs
            ldx #30
            ldy #0
            jsr PrintStr
            ldy #0
@loop:      lda LoadCmd,Y       ; Write in the keyboard buffer
            sta 631,Y
            iny
            cpy #LoadCmdLen
            bne @loop
            lda #LoadCmdLen
            sta 198
            rts

@cassette:
@disk8:
@disk9:     clc
            adc #48
            jmp @loadc

@disk10:    lda #49
            jsr DrawChar
            inx
            lda #48
            jmp @loadc

@disk11:    lda #49
            jsr DrawChar
            inx
            lda #49
            jmp @loadc

; INIT - INIT - INIT - INIT - INIT - INIT - INIT - INIT - INIT - INIT - INIT
;
; Initialization code: prepare the screen to the correct size, center it and
; load the graphic chars and configure the IRQ handler.
;
; INIT - INIT - INIT - INIT - INIT - INIT - INIT - INIT - INIT - INIT - INIT



; Synchronize raster on PAL systems

SyncPAL:
            ; Data for PAL machines. See for example:
            ; http://www.antimon.org/dl/c64/code/stable.txt
            LINES_PAL = 312
            CYCLES_PER_LINE_PAL = 71
            TIMER_VALUE_PAL = LINES_PAL * CYCLES_PER_LINE_PAL - 2
            lda #120
            sta RasterlNMI
@loopsync:  lda VICRAST     ; Synchronization loop
            cmp #60
            bne @loopsync
            lda #<TIMER_VALUE_PAL
            ldx #>TIMER_VALUE_PAL
            jmp ContInit

SyncNTSC:
            ; Data for NTSC machines. See for example:
            ; http://www.antimon.org/dl/c64/code/stable.txt
            LINES_NTSC = 261
            CYCLES_PER_LINE_NTSC = 65
            TIMER_VALUE_NTSC = LINES_NTSC * CYCLES_PER_LINE_NTSC - 2
            lda #100
            sta RasterlNMI
@loopsync:  lda VICRAST     ; Synchronization loop
            cmp #80
            bne @loopsync
            lda #<TIMER_VALUE_NTSC
            ldx #>TIMER_VALUE_NTSC
            jmp ContInit


Init:       jsr MovCh64
            lda #$FF        ; Move the character generator address to $1C00
            sta VICCHGEN    ; while leaving ch. 128-255 to their original pos.
            lda #$0A
            sta VICCOLOR
            lda #$70        ; Turn on the volume, set multicolour add. colour 2
            sta VOLUME
            cmp #$05        ; Determine if we run on a PAL or NTSC machine
            beq SyncNTSC    ; Load the screen settings
            bne SyncPAL
ContInit:   sta T1LLVIA2
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
            jsr CopyColours
            jsr CopyScreen

            lda PORTAVIA1d
            sta portaconfig
            lda #0          ; Many thanks to @BedfordLvlExp for the joystick
            sta PORTAVIA1d  ; code here!
            rts

; and NTSC


; Synchronize the timer to the NMI interrupt to a given raster line.

SyncLater:
            lda RasterlNMI  ; Synchronization loop
@loopsync:  cmp VICRAST
            bne @loopsync
            rts


; Copy the graphic chars. All the 64 available on the unexpanded VIC.

MovCh64:    ldx #255
@loop:      lda char_data,x
            sta GRCHARS1,x
            lda char_data+256,x
            sta GRCHARS1+256,x
            dex
            cpx #255
            bne @loop
            rts

CopyColours:ldy #255
@CopyMem:   lda colour_data,y
            sta MEMCLR,y
            lda colour_data+256,y
            sta MEMCLR+256,y
            dey
            cpy #255
            bne @CopyMem

            rts

CopyScreen: ldy #255
@CopyMem:   lda screen_data,y
            sta MEMSCR,y
            lda screen_data+256,y
            sta MEMSCR+256,y
            dey
            cpy #255
            bne @CopyMem
            rts

DrawChar:   sta CharCode
            stx PosX
            sty PosY
            tya
            asl             ; 16 columns per line. Multiply!
            asl
            asl
            asl             ; If it shifts an 1 in the carry, this means that
            lda secs
            cmp #$01
            bne @tophalf    ; we need to write in the bottom-half of the screen
            clc
            adc PosX
            tay
            lda Colour
            sta MEMCLR+256,Y
            lda CharCode
            sta MEMSCR+256,Y
            jmp @exit
@tophalf:   adc PosX
            tay
            lda Colour
            sta MEMCLR,Y
            lda CharCode
            sta MEMSCR,Y
@exit:      ldy PosY
            rts
; NMI - NMI - NMI - NMI - NMI - NMI - NMI - NMI - NMI - NMI - NMI - NMI - NMI
;
; This is the NMI handler, called 50 times each second when the VIC-20
; is a PAL unit or 60 when NTSC.
;
; NMI - NMI - NMI - NMI - NMI - NMI - NMI - NMI - NMI - NMI - NMI - NMI - NMI

NMIHandler: pha
            lda #202
            sta VICCOLOR
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

IrqHandler: pha
            txa             ; Save registers
            pha
            tya
            pha

            lda #8
            sta VICCOLOR

            pla             ; Restore registers
            tay
            pla
            tax
            pla
            jmp $EABF       ; Jump to the standard IRQ handling routine

; Print a string (null terminated) whose address is contained in str1 and
; str1+1 at the position given by X and Y pointers

PrintStr:   sty PosY
            ldy #106
@loop:      lda (str1),Y
            beq @exit
            sty tmp4
            ldy PosY
            jsr DrawChar
            ldy tmp4
            iny
            inx
            jmp @loop
@exit:      ldy PosY
            rts

; Clear the screen. This maybe is too slow to be used in an interrupt handler.
; Draw everywhere the character contained in the A register. Employs X.

CLS:
            size=22*23/4+1
            ldx #size
@loop:      sta MEMSCR-1,X          ; A (small) degree of loop unrolling avoids
            sta MEMSCR+size-1,X     ; to mess with a 16-bit loop.
            sta MEMSCR+size*2-1,X
            sta MEMSCR+size*3-1,X
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
            dex
            bne @loop
            rts



; VIC-20 screen editor: http://www.fox-ts.co.uk/vic20sdd/Vic20SDD.htm

;settings
;background-colour=0
;border-colour=2
;aux-colour=7
;char-height=8
;row-count=23
;col-count=22


char_data:
.byte  $00,$00,$00,$00,$00,$00,$00,$00
.byte  $00,$01,$8F,$9F,$BF,$BF,$BF,$BF
.byte  $7F,$FF,$FF,$FF,$FF,$FF,$FF,$FF
.byte  $00,$00,$00,$00,$00,$01,$07,$1F
.byte  $BF,$BF,$BF,$BF,$BF,$BF,$BF,$BF
.byte  $BE,$F6,$FE,$DE,$FC,$EC,$7C,$DC
.byte  $B6,$BF,$9D,$0F,$07,$03,$01,$00
.byte  $FC,$78,$38,$18,$08,$08,$08,$08
.byte  $08,$10,$10,$10,$10,$10,$10,$08
.byte  $08,$08,$08,$08,$08,$08,$04,$04
.byte  $04,$04,$04,$04,$02,$02,$02,$02
.byte  $02,$02,$02,$01,$01,$01,$01,$01
.byte  $80,$80,$40,$40,$20,$20,$10,$08
.byte  $04,$03,$00,$00,$00,$00,$00,$00
.byte  $7F,$FE,$38,$00,$00,$00,$00,$00
.byte  $07,$07,$0F,$0F,$1F,$1F,$3F,$3F
.byte  $F4,$B8,$D8,$E8,$B0,$E0,$40,$80
.byte  $03,$03,$03,$03,$03,$03,$07,$07
.byte  $AB,$AF,$BF,$BF,$B3,$FC,$FC,$FC
.byte  $01,$01,$01,$01,$01,$01,$01,$03
.byte  $C0,$C0,$C0,$80,$80,$80,$00,$00
.byte  $03,$01,$01,$01,$01,$01,$01,$01
.byte  $60,$E0,$A0,$E0,$E0,$40,$C0,$C0
.byte  $07,$07,$07,$07,$03,$03,$03,$03
.byte  $FD,$FE,$FE,$FF,$FF,$FF,$FF,$FF
.byte  $FF,$EF,$FC,$F0,$00,$E0,$E0,$E0
.byte  $3F,$1F,$1F,$1F,$0F,$0F,$0F,$0F
.byte  $F7,$F7,$FB,$FB,$FB,$FB,$FB,$FB
.byte  $FF,$FF,$FD,$FF,$F7,$DF,$7C,$E0
.byte  $FF,$FF,$FF,$7F,$7F,$7F,$7F,$3F
.byte  $FD,$AF,$FF,$FF,$FF,$E1,$00,$00
.byte  $EF,$EF,$EF,$EF,$F7,$F7,$F7,$F7
.byte  $E8,$E8,$E8,$E8,$E8,$C8,$08,$00
.byte  $E8,$E8,$E8,$E8,$E8,$E8,$E8,$E8
.byte  $04,$04,$3C,$00,$17,$01,$01,$32
.byte  $FF,$FF,$EF,$FF,$EF,$FF,$EF,$EF
.byte  $38,$9E,$E3,$5E,$CF,$FF,$BB,$7F
.byte  $00,$00,$00,$00,$00,$88,$E8,$E8
.byte  $80,$C0,$F0,$FC,$FF,$FF,$FF,$FF
.byte  $03,$24,$28,$49,$10,$10,$20,$2A
.byte  $00,$00,$00,$00,$C0,$F0,$FC,$FE
.byte  $7F,$FD,$DF,$67,$A8,$AA,$AA,$AA
.byte  $FF,$FF,$FD,$F6,$DA,$AA,$AA,$AA
.byte  $7D,$5F,$FD,$3F,$A3,$AB,$A8,$AA
.byte  $E0,$F8,$FE,$FF,$FF,$FF,$FF,$FF
.byte  $20,$10,$E0,$E0,$C0,$00,$C0,$00
.byte  $CC,$E8,$F0,$F0,$F8,$D0,$D0,$C8
.byte  $C0,$24,$14,$92,$08,$08,$04,$54
.byte  $C0,$CC,$C0,$50,$60,$4C,$F0,$F0
.byte  $00,$00,$20,$C0,$C0,$C8,$F0,$C0
.byte  $0C,$22,$02,$0F,$03,$03,$02,$03
.byte  $0C,$0E,$02,$0C,$02,$02,$03,$03
.byte  $04,$00,$00,$0D,$00,$03,$04,$03
.byte  $FE,$FB,$FF,$FE,$FB,$FF,$FA,$FB
.byte  $DC,$2F,$C3,$7F,$C1,$7D,$3F,$F3
.byte  $76,$5E,$37,$D1,$75,$FF,$51,$FC
.byte  $10,$0D,$4C,$15,$7C,$7E,$1F,$FF
.byte  $3C,$42,$C0,$8E,$80,$C4,$38,$C0
.byte  $15,$F8,$3C,$BC,$FF,$BF,$E0,$CC
.byte  $08,$00,$08,$00,$00,$08,$08,$00
.byte  $DF,$FF,$DF,$FF,$E7,$C3,$83,$81
.byte  $54,$04,$08,$08,$92,$14,$24,$C0
.byte  $2A,$20,$10,$10,$49,$28,$24,$03
.byte  $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF

;screen data0

screen_data:
.byte  $00,$00,$00,$00,$00,$00,$00,$00
.byte  $00,$00,$00,$00,$00,$00,$00,$00
.byte  $00,$00,$00,$00,$27,$2F,$00,$00
.byte  $00,$00,$00,$00,$00,$00,$00,$00
.byte  $00,$00,$3B,$00,$3B,$00,$38,$00
.byte  $3B,$00,$3E,$3D,$00,$00,$83,$81
.byte  $96,$85,$92,$8E,$00,$00,$00,$00
.byte  $00,$00,$00,$22,$37,$22,$00,$00
.byte  $3B,$3B,$00,$00,$00,$00,$00,$00
.byte  $00,$00,$00,$00,$00,$00,$00,$3B
.byte  $34,$36,$24,$36,$31,$00,$00,$00
.byte  $00,$00,$00,$85,$98,$90,$8C,$8F
.byte  $92,$85,$92,$00,$00,$00,$22,$36
.byte  $37,$24,$30,$00,$3B,$00,$00,$00
.byte  $00,$00,$00,$00,$00,$00,$00,$00
.byte  $00,$00,$00,$00,$32,$24,$36,$24
.byte  $39,$00,$00,$00,$00,$00,$00,$00
.byte  $00,$00,$00,$00,$00,$00,$00,$00
.byte  $00,$3B,$33,$3A,$24,$36,$2E,$3B
.byte  $00,$3B,$00,$00,$00,$00,$00,$00
.byte  $00,$00,$00,$00,$00,$00,$00,$00
.byte  $03,$29,$2A,$2B,$2D,$00,$00,$00
.byte  $00,$00,$8D,$AD,$93,$86,$98,$00
.byte  $8F,$86,$86,$00,$00,$01,$02,$3F
.byte  $3F,$3F,$2C,$28,$3B,$00,$00,$00
.byte  $00,$00,$00,$00,$00,$00,$00,$00
.byte  $00,$00,$00,$04,$3F,$3F,$3F,$3F
.byte  $3F,$3F,$26,$25,$00,$00,$00,$00
.byte  $00,$00,$93,$00,$00,$00,$00,$00
.byte  $00,$04,$3F,$3C,$3F,$23,$3F,$3F
.byte  $3F,$21,$00,$00,$00,$00,$00,$00
.byte  $DD,$00,$00,$00,$00,$00,$00,$06
.byte  $05,$00,$1D,$1F,$3F,$3F,$3F,$21
.byte  $00,$00,$00,$00,$9A,$C0,$F1,$C0
.byte  $97,$00,$00,$00,$00,$00,$07,$00
.byte  $1A,$1B,$3F,$1C,$1E,$20,$00,$00
.byte  $00,$00,$00,$00,$00,$00,$00,$00
.byte  $00,$00,$00,$00,$08,$00,$17,$18
.byte  $19,$00,$00,$00,$00,$00,$00,$00
.byte  $00,$00,$00,$00,$00,$00,$00,$00
.byte  $00,$00,$09,$00,$15,$3F,$16,$00
.byte  $00,$00,$00,$00,$00,$00,$00,$8F
.byte  $92,$00,$00,$00,$00,$00,$00,$00
.byte  $0A,$00,$13,$35,$14,$00,$00,$00
.byte  $00,$00,$00,$00,$00,$00,$00,$00
.byte  $00,$00,$00,$00,$00,$00,$0B,$00
.byte  $11,$12,$00,$00,$27,$2F,$00,$00
.byte  $8A,$8F,$99,$93,$94,$89,$83,$8B
.byte  $00,$00,$00,$00,$00,$0C,$0F,$10
.byte  $00,$00,$3E,$3D,$00,$00,$00,$00
.byte  $00,$00,$00,$00,$00,$00,$00,$00
.byte  $00,$27,$2F,$0D,$0E,$00,$00,$00
.byte  $00,$00,$00,$00,$00,$00,$00,$00
.byte  $00,$00,$00,$00,$00,$00,$00,$3E
.byte  $3D,$00,$00,$00,$00,$00,$00,$00
.byte  $00,$00,$00,$00,$00,$00,$00,$00
.byte  $00,$00,$00,$00,$00,$00,$00,$00
.byte  $00,$00,$00,$00,$00,$00,$00,$00
.byte  $00,$00,$00,$00,$00,$00,$00,$84
.byte  $AE,$00,$82,$95,$83,$83,$89,$00
.byte  $B2,$B0,$B1,$B8,$00,$00,$00,$00
.byte  $00,$00,$00,$00,$00,$00,$00,$00
.byte  $00,$00,$00,$00,$00,$00,$00,$00
.byte  $00,$00

;colour data0

colour_data:
.byte  $01,$01,$01,$01,$01,$01,$01,$01
.byte  $01,$01,$01,$01,$01,$01,$01,$01
.byte  $01,$01,$01,$01,$03,$03,$01,$01
.byte  $01,$01,$01,$01,$01,$01,$01,$01
.byte  $01,$01,$06,$01,$06,$01,$0C,$01
.byte  $06,$01,$03,$03,$01,$01,$01,$01
.byte  $01,$01,$01,$01,$01,$01,$01,$01
.byte  $01,$01,$01,$0D,$0C,$0F,$01,$01
.byte  $06,$06,$01,$01,$01,$01,$01,$01
.byte  $01,$01,$01,$01,$01,$01,$01,$06
.byte  $0C,$0D,$0F,$0D,$0C,$01,$01,$01
.byte  $01,$01,$01,$01,$01,$01,$01,$01
.byte  $01,$01,$01,$01,$01,$01,$0C,$0D
.byte  $0C,$0F,$0C,$01,$06,$01,$01,$01
.byte  $01,$01,$01,$01,$01,$01,$01,$01
.byte  $01,$01,$01,$01,$0C,$0D,$0D,$0D
.byte  $0C,$01,$01,$01,$01,$01,$01,$01
.byte  $01,$01,$01,$01,$01,$01,$01,$01
.byte  $01,$06,$0D,$0D,$0D,$0D,$0D,$06
.byte  $01,$06,$01,$01,$01,$01,$01,$01
.byte  $01,$01,$01,$01,$01,$01,$01,$01
.byte  $01,$09,$09,$09,$0D,$01,$01,$01
.byte  $01,$01,$07,$07,$07,$07,$07,$01
.byte  $07,$07,$07,$01,$01,$01,$01,$01
.byte  $01,$01,$01,$01,$06,$01,$01,$01
.byte  $01,$01,$01,$01,$01,$01,$01,$01
.byte  $01,$01,$01,$01,$01,$01,$01,$01
.byte  $01,$01,$01,$01,$01,$01,$01,$01
.byte  $01,$01,$07,$01,$01,$01,$01,$01
.byte  $01,$01,$01,$01,$01,$01,$01,$01
.byte  $01,$01,$01,$01,$01,$01,$01,$01
.byte  $07,$01,$01,$01,$01,$01,$01,$01
.byte  $01,$01,$01,$01,$01,$01,$01,$01
.byte  $01,$01,$01,$01,$07,$07,$07,$07
.byte  $07,$01,$01,$01,$01,$01,$01,$01
.byte  $01,$01,$01,$01,$01,$01,$01,$01
.byte  $01,$01,$01,$01,$01,$01,$01,$01
.byte  $01,$01,$01,$01,$01,$01,$01,$01
.byte  $01,$01,$01,$01,$01,$01,$01,$01
.byte  $01,$01,$01,$01,$01,$01,$01,$01
.byte  $01,$01,$01,$01,$01,$01,$07,$01
.byte  $01,$01,$01,$01,$01,$01,$01,$07
.byte  $07,$01,$01,$01,$01,$01,$01,$01
.byte  $01,$01,$01,$01,$07,$01,$01,$01
.byte  $01,$01,$01,$01,$01,$01,$01,$01
.byte  $01,$01,$01,$01,$01,$01,$01,$01
.byte  $01,$09,$01,$01,$03,$03,$01,$01
.byte  $07,$07,$07,$07,$07,$07,$07,$07
.byte  $01,$01,$01,$01,$01,$01,$07,$07
.byte  $01,$01,$03,$03,$01,$01,$01,$01
.byte  $01,$01,$01,$01,$01,$01,$01,$01
.byte  $01,$03,$03,$07,$07,$01,$01,$01
.byte  $01,$01,$01,$01,$01,$01,$01,$01
.byte  $01,$01,$01,$01,$01,$01,$01,$03
.byte  $03,$01,$01,$01,$01,$01,$01,$01
.byte  $01,$01,$01,$01,$01,$01,$01,$01
.byte  $01,$01,$01,$01,$01,$01,$01,$01
.byte  $01,$01,$01,$01,$01,$01,$01,$01
.byte  $01,$01,$01,$01,$01,$01,$01,$01
.byte  $01,$01,$01,$01,$01,$01,$01,$01
.byte  $01,$01,$01,$01,$01,$01,$01,$01
.byte  $01,$01,$01,$01,$01,$01,$01,$01
.byte  $01,$01,$01,$01,$01,$01,$01,$01
.byte  $01,$01

LoadStr:    .byte " ",('l'-'@'),('o'-'@'),('a'-'@'),('d'-'@'),34,('e'-'@')
            .byte ('x'-'@'),('p'-'@'),('l'-'@'),('o'-'@'),45
            .byte ('r'-'@'),('e'-'@'),('r'-'@'),34,44,0

EndL:       .byte "                      "
            .byte "                      "
            .byte "                      "
            .byte "                      "
            .byte "                      "
            .byte "     "
RunStr:     .byte ('r'-'@'),('u'-'@'),('n'-'@'),0
Loading:    .byte ('L'-'@'),('O'-'@'),('A'-'@'),('D'-'@'),('I'-'@')
            .byte ('N'-'@'),('G'-'@'),0
LoadCmdLen=5
LoadCmd:    .byte 131,13,13