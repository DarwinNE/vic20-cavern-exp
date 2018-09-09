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

; Reference address to test for a NTSC or PAL machine
        INITVALC=$ede4      ; if it is 5 it is NTSC

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

        VOICE1  = GEN3      ; Voice 1 for music
        VOICE2  = GEN2      ; Voice 2 for music
        VOICE3  = GEN1      ; Voice 3 for music

        

        terminator1 = $07   ; Used in PRNSTRN
        terminator2 = $08   ; Used in PRNSTRN

        str1      = $9      ; Address of the string to print with PrintStr (w.)
        portaconfig=$C
        idleloops = $D      ; Number of idle loops for PAL/NTSC sync
        IRQcounter= $E      ; Free running IRQ counter

        RasterlNMI= $53     ; Raster line for the NMI interrupt (b.)
        
        ; KERNAL routines used

        GETIN =  $FFE4      ; Get a key from the keyboard
        PRNSTR = $CB1E      ; Print " or 0 terminated string
        CLRHOME= $E55F      ; Clear screen and home cursor
        PRNSTRN= $D48D      ; Print a string terminated by terminator1 or 2
        PUTCH  = $CB47      ; Print the PETSCII char in A

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
            lda #<$FEAD
            sta $0318
            lda #>$FEAD
            sta $0319
            cli
            jsr CLRHOME         ; Clear the screen and home cursor
            lda #$0A
            sta VICCOLOR
            lda #240
            sta VICCHGEN
            
            lda #<LoadStr1
            ldy #>LoadStr1      ; Write a load command
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

@loadc:     jsr PUTCH       ; Complete the load command
            lda #<MoveCrsr
            ldy #>MoveCrsr
            jsr PrintStr
            ldy #0
@loop:      lda LoadCmd,Y    ; Write in the keyboard buffer
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
            jsr PUTCH
            lda #48
            jmp @loadc

@disk11:    lda #49
            jsr PUTCH
            lda #49
            jmp @loadc


; Print a string (null terminated) whose address is contained in str1 and
; str1+1

PrintStr:   sta str1
            sty str1+1
            ldy #0
@loop:      lda (str1),Y
            beq @exit
            jsr PUTCH
            iny
            jmp @loop
@exit:      rts


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
            lda #6
            sta idleloops
@loopsync:  lda VICRAST     ; Synchronization loop
            cmp #76
            bne @loopsync
            lda #<TIMER_VALUE_PAL
            ldx #>TIMER_VALUE_PAL
            nop
            nop
            jmp ContInit

SyncNTSC:
            ; Data for NTSC machines. See for example:
            ; http://www.antimon.org/dl/c64/code/stable.txt
            LINES_NTSC = 261
            CYCLES_PER_LINE_NTSC = 65
            TIMER_VALUE_NTSC = LINES_NTSC * CYCLES_PER_LINE_NTSC - 2
            lda #107
            sta RasterlNMI
            lda #12
            sta idleloops
@loopsync:  lda VICRAST     ; Synchronization loop
            cmp #62
            bne @loopsync
            lda #<TIMER_VALUE_NTSC
            ldx #>TIMER_VALUE_NTSC
            nop
            nop
            nop
            nop
            nop
            nop
            nop
            nop
            nop
            nop
            nop
            nop
            nop
            nop
            nop
            nop
            jmp ContInit


Init:       jsr MovCh64
            lda #$FF        ; Move the character generator address to $1C00
            sta VICCHGEN    ; while leaving ch. 128-255 to their original pos.
            lda #$0A
            sta VICCOLOR
            lda #$75        ; Turn on the volume, set multicolour add. colour 2
            sta VOLUME
            lda INITVALC
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
            sta Repet1
            sta Repet2
            sta Repet3
            rts

; and NTSC


; Synchronize the timer to the NMI interrupt to a given raster line.

SyncLater:
            lda RasterlNMI  ; Synchronization loop
@loopsync:  cmp VICRAST
            bne @loopsync
            ldy idleloops
@idle:      dey
            bne @idle
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
            lda #8
            sta VICCOLOR
            txa             ; Save registers
            pha
            tya
            pha
            inc IRQcounter
            
            jsr Music1
            jsr Music2

            lda Repet1
            beq @skip1
            jsr Music3
            
@skip1:
            pla             ; Restore registers
            tay
            pla
            tax
            pla
            jmp $EABF       ; Jump to the standard IRQ handling routine


; Music driver for voice 1. It should be called every IRQ to handle music

Music1:     ldy Voice1ctr
            beq @playnext
            cpy Voice1nod
            bne @dec
            lda #$00
            sta VOICE1
            dey
            sty Voice1ctr
            rts
@dec:       dey
            sty Voice1ctr
            lda IRQcounter
            and #4
            bne @octavesup
            ldy CurrNote1
            lda octave,y
            sta VOICE1
            rts
@octavesup: lda CurrNote1
            clc
            adc #7
            tay
            lda octave,y
            sta VOICE1
            rts

@playnext:  ldx Voice1ptr
            lda Voice1data,x
            cmp #repeatm
            beq @repeat
            and #maskcode
            cmp #notecode
            beq @note
            cmp #duracode
            beq @duration

@exitmusic: inx
            stx Voice1ptr
            rts

@note:      ldy Voice1data,x
            sty CurrNote1
            lda octave,y
            sta VOICE1
            ;sta VOICE3
            ldy Voice1drt
            sty Voice1ctr
            jmp @exitmusic

@duration:  lda Voice1data,x
            and #unmask
            sta Voice1drt
            inx
            lda Voice1data,x
            sta Voice1nod
            inx
            stx Voice1ptr
            jmp @playnext

@repeat:    ldx #0
            stx Voice1ptr
            stx Voice1ctr
            inc Repet1
            jmp @playnext
            

; Music driver for voice 2. It should be called every IRQ to handle music

Music2:     ldy Voice2ctr
            beq @playnext
            cpy Voice2nod
            bne @dec
            lda #$00
            sta VOICE2
            sta VOICE3
            dey
            sty Voice2ctr
            rts
@dec:       dey
            sty Voice2ctr
            lda IRQcounter
            clc
            adc #1
            and #4
            bne @octavesup
            ldy CurrNote2
            lda octave,y
            sta VOICE2
            sta VOICE3
            rts
@octavesup: lda CurrNote2
            clc
            adc #7
            tay
            lda octave,y
            sta VOICE2
            sta VOICE3
            rts

@playnext:  ldx Voice2ptr
            lda Voice2data,x
            cmp #repeatm
            beq @repeat
            and #maskcode
            cmp #notecode
            beq @note
            cmp #duracode
            beq @duration

@exitmusic: inx
            stx Voice2ptr
            rts

@note:      ldy Voice2data,x
            sty CurrNote2
            lda octave,y
            sta VOICE2
            sta VOICE3
            ldy Voice2drt
            sty Voice2ctr
            jmp @exitmusic

@duration:  lda Voice2data,x
            and #unmask
            sta Voice2drt
            inx
            lda Voice2data,x
            sta Voice2nod
            inx
            stx Voice2ptr
            jmp @playnext

@repeat:    ldx #0
            stx Voice2ptr
            stx Voice2ctr
            inc Repet2
            jmp @playnext
         
; Music driver for voice 3. It should be called every IRQ to handle music

Music3:     ldy Voice3ctr
            beq @playnext
            cpy Voice3nod
            bne @dec
            lda #$00
            sta NOISE
@dec:       dey
            sty Voice3ctr
            ldy Envelopec
            lda envelope,y
            beq @fullvol
            ora #$70
            sta VOLUME
            inc Envelopec
            rts
@fullvol:
            lda #$75
            sta VOLUME
            rts
@playnext:  ldx Voice3ptr
            lda Voice3data,x
            cmp #repeatm
            beq @repeat
            and #maskcode
            cmp #notecode
            beq @note
            cmp #duracode
            beq @duration

@exitmusic: inx
            stx Voice3ptr
            rts

@note:      ldy Voice3data,x
            sty CurrNote3
            lda octave,y
            sta NOISE
            lda #0
            sta Envelopec
            ldy Voice3drt
            sty Voice3ctr
            jmp @exitmusic

@duration:  lda Voice3data,x
            and #unmask
            sta Voice3drt
            inx
            lda Voice3data,x
            sta Voice3nod
            inx
            stx Voice3ptr
            jmp @playnext

@repeat:    ldx #0
            stx Voice3ptr
            stx Voice3ctr
            inc Repet3
            jmp @playnext

; DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA
;
; Data and configuration settings.
;
; DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA - DATA

; Music data

notecode = %00000000
duracode = %10000000
repeatm  = %11111111
maskcode = %10000000
unmask   = %01111111

VoiceBase:  .byte $00

Repet1:     .byte $00
Voice1ptr:  .byte $00
Voice1ctr:  .byte $00
Loop1ctr:   .byte $00
Loop1str:   .byte $00
Voice1drt:  .byte $00
Voice1nod:  .byte $00
CurrNote1:  .byte $00

Repet2:     .byte $00
Voice2ptr:  .byte $00
Voice2ctr:  .byte $00
Loop2ctr:   .byte $00
Loop2str:   .byte $00
Voice2drt:  .byte $00
Voice2nod:  .byte $00
CurrNote2:  .byte $00

Repet3:     .byte $00
Voice3ptr:  .byte $00
Voice3ctr:  .byte $00
Loop3ctr:   .byte $00
Loop3str:   .byte $00
Voice3drt:  .byte $00
Voice3nod:  .byte $00
CurrNote3:  .byte $00
Envelopec:  .byte $00


do0=0
dod0=1
re0=2
red0=3
mi0=4
fa0=5
fad0=6
sol0=7
sold0=8
la0=9
lad0=10
si0=11
do1=12
dod1=13
re1=14
red1=15
mi1=16
fa1=17
fad1=18
sol1=19
sold1=20
la1=21
lad1=22
si1=23
do2=24
dod2=25
re2=26
red2=27
mi2=28
fa2=29
fad2=30        ; Quite out of tune on higher pitches
sol2=31        ; ...
sold2=32       ;
la2=33
lad2=34
si2=35
do3=36
dod3=37
silence=38

octave: 
.byte 128,134,141,147,153,159,164,170,174,179
.byte 183,187,191,195,198,201,204,207,210,213,215,217
.byte 219,221,223,225,227,229,230,231,232,234,235,236,237,238,239,240,0

envelope:
.byte 10,15,10,9,7,5,4,3,2,1,0

; Quite out of tune on higher pitches,

quaver = 31
quaverd = 20

semiquaver = 15
semiquaverd = 10

semisemiquaver = 7
semisemiquaverd= 4

; Music data

Voice1data: ; Measures 1 - 8
            .byte duracode + semiquaver, semiquaverd
            .byte la0,si0,do1,re1 
            .byte la0,si0,do1,re1
            .byte la0,si0,do1,re1
            .byte la0,si0,do1,re1
            
            .byte fa0,sol0,la0,si0
            .byte fa0,sol0,la0,si0
            .byte la0,si0,do1,re1
            .byte la0,si0,do1,re1
            
            .byte sol0,la0,si0,do1 
            .byte sol0,la0,si0,do1 
            .byte la0,si0,do1,re1 
            .byte la0,si0,do1,re1

            .byte re1,mi1,fa1,sol1 
            .byte re1,mi1,fa1,sol1 
            .byte re1,mi1,fa1,sol1 
            .byte re1,mi1,fa1,sol1 

            .byte repeatm
            
Voice2data: ; Measures 1 - 8
            .byte duracode + semiquaver, semiquaverd
            .byte mi1,mi1,mi1,mi1
            .byte mi1,mi1,mi1,mi1
            .byte mi1,mi1,mi1,mi1
            .byte mi1,mi1,mi1,mi1
           
            .byte fa1,fa1,fa1,fa1
            .byte fa1,fa1,fa1,fa1
            .byte mi1,mi1,mi1,mi1
            .byte mi1,mi1,mi1,mi1
            
            .byte re1,re1,re1,sol1
            .byte re1,re1,re1,sol1
            .byte fa1,sol1,la1,si1
            .byte fa1,sol1,la1,si1
            
            .byte sol1,sol1,sol1,sol1
            .byte sol1,sol1,sol1,sol1
            .byte sol1,sol1,sol1,sol1
            .byte sol1,sol1,sol1,sol1
            
            
            .byte repeatm

Voice3data: .byte duracode + quaver, 20
            .byte do0
            .byte duracode + semiquaver, 10
            .byte do0,do0
            .byte duracode + semisemiquaver, 6
            .byte do3,do3,do3,do3
            .byte duracode + quaver, 20
            .byte do0
            .byte repeatm


; VIC-20 screen editor: http://www.fox-ts.co.uk/vic20sdd/Vic20SDD.htm

;settings
;background-colour=0
;border-colour=2
;aux-colour=7
;char-height=8
;row-count=23
;col-count=22


;char data
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
.byte  $02,$02,$02,$01,$01,$01,$01,$00
.byte  $80,$80,$80,$40,$20,$20,$10,$08
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
.byte  $E8,$E8,$E8,$E8,$F8,$D8,$08,$00
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
.byte  $80,$00,$C0,$40,$00,$40,$40,$00
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
.byte  $60,$20,$00,$30,$30,$00,$10,$00
.byte  $08,$00,$08,$00,$00,$08,$08,$00
.byte  $DF,$FF,$DF,$FF,$E7,$C3,$83,$81
.byte  $54,$04,$08,$08,$92,$14,$24,$C0
.byte  $2A,$20,$10,$10,$49,$28,$24,$03
.byte  $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF


;screen data0
screen_data:
.byte  $00,$00,$00,$00,$00,$00,$00,$00
.byte  $00,$00,$00,$00,$00,$00,$00,$00
.byte  $00,$00,$00,$00,$00,$00,$00,$00
.byte  $00,$00,$00,$00,$00,$00,$00,$00
.byte  $00,$00,$3B,$00,$3B,$00,$38,$00
.byte  $3B,$00,$00,$00,$00,$00,$83,$81
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
.byte  $00,$3B,$33,$36,$24,$36,$2E,$3B
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
.byte  $98,$00,$00,$00,$00,$00,$07,$00
.byte  $1A,$1B,$3F,$1C,$1E,$20,$00,$00
.byte  $00,$00,$00,$00,$00,$00,$00,$00
.byte  $00,$00,$00,$00,$08,$00,$17,$18
.byte  $19,$00,$00,$00,$00,$00,$00,$00
.byte  $00,$00,$00,$00,$00,$00,$00,$00
.byte  $00,$00,$09,$2E,$15,$3F,$16,$00
.byte  $00,$00,$00,$00,$00,$00,$00,$8F
.byte  $92,$00,$00,$00,$00,$00,$00,$00
.byte  $0A,$3A,$13,$35,$14,$00,$27,$2F
.byte  $00,$00,$00,$00,$00,$00,$00,$00
.byte  $00,$00,$00,$00,$00,$00,$0B,$00
.byte  $11,$12,$00,$00,$3E,$3D,$00,$00
.byte  $8A,$8F,$99,$93,$94,$89,$83,$8B
.byte  $00,$00,$00,$00,$20,$0C,$0F,$10
.byte  $00,$00,$00,$00,$00,$00,$00,$00
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
.byte  $01,$01,$00,$00,$00,$00,$00,$01
.byte  $00,$00,$00,$01,$01,$01,$01,$01
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
.byte  $01,$01,$01,$03,$01,$01,$07,$01
.byte  $01,$01,$01,$01,$01,$01,$01,$07
.byte  $07,$01,$01,$01,$01,$01,$01,$01
.byte  $01,$03,$01,$01,$07,$01,$03,$03
.byte  $01,$01,$01,$01,$01,$01,$01,$01
.byte  $01,$01,$01,$01,$01,$01,$01,$01
.byte  $01,$09,$01,$01,$03,$03,$01,$01
.byte  $07,$07,$07,$07,$07,$07,$07,$07
.byte  $01,$01,$01,$01,$00,$01,$07,$07
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


LoadStr1:   .byte 17,17,"load",34,"explorer",34,44,0


RunStr:     .byte "run",0
Loading:    .byte "loading...",0
LoadCmdLen=3
LoadCmd:    .byte 131,13,13
MoveCrsr:   .byte 145,145,0