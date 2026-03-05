;**************************************
; CONSTANTS
.eq SCREEN_H 32
.eq SCREEN_W 16
.eq START_LINE $57       ; first line
.eq LINES 261            ; 312 for PAL
.eq CYCLES_PER_LINE 65   ; 71 for PAL
.eq TIMER_VALUE LINES*CYCLES_PER_LINE-2

.eq SCREEN $1e00
.eq COLORMEM $9600

;**************************************
; PLASMA PARAMS
.eq WIDTH 16
.eq HEIGHT 16
.eq SINSPREADX 3
.eq SINSPREADY 1
.eq COLORSPREADX 1
.eq COLORSPREADY 2
.eq RTSPREAD0 4
.eq RTSPREAD1 7

;**************************************
; ZEROPAGE
.eq plasmaCnt $fa
.eq add $fc
.eq yPos $fd
.eq plasmaCntCpy $fe

.org $1001
;.dw $100b
;.dw 2011
;.db $9e
;.db "4109",0
;.dw 0

start
plasma
    ; init
    lda #$05       ; vertical centering
    sta $9001
    lda #SCREEN_H.1 ; # of rows
    sta $9003
    lda #SCREEN_W.$80   ; # of columns
    sta $9002
    lda #$fd
    sta $9005
    lda #$c0
    sta $900e       ; auxiliary color
    lda #$eb
    sta $900f       ; screen/border
    lda #SCREEN_W.$80
    sta $9002

    lda #$00
    sta add
    sta plasmaCnt
    sta plasmaCnt+1

    ldx #$00
genColorTab:
    txa
    asl
    asl
    asl
bcc +
    eor #$ff
:   lsr
    lsr
    lsr
    lsr
    tay
    lda colors,y
    sta colorTable,x
    sta colorTable+$100,x
    lda chars,y
    sta charTable,x
    sta charTable+$100,x
    inx
    bne genColorTab

    ; init the stable raster irq
    lda #$7f
    sta $912e  ; disable/ack interrupts
    sta $912d
    ;sta $911e  ; disable NMI's
sync:
; sync with screen
    ; wait until this raster (*2)
    ldx #START_LINE
:   cpx $9004
    bne -

    ldy #$09
    bit $24
:   ldx $9004
    txa
    bit $24

    bit $24
    ldx #21

    dex
    bne *-1
    cmp $9004
    bcs *+2
    dey
    bne -

; 6 cycles have passed since last
; $9004 change we are now on line
; 2*(START_LINE+9)

timers:
; initialize the timers
; enable timer A free run on both VIAs
    lda #$40
    sta $911b
    sta $912b

    lda #<TIMER_VALUE
    ldx #>TIMER_VALUE
    ; load the timer low byte latches
    sta $9116
    sta $9126

    ; delay to get effect @ right spot
    ldy #$06
    dey
    bne *-1
    bit $24

    stx $9125       ; start IRQ timer A
                    ; NTSC: 65 cycles
                    ; PAL: 77 cycles

    ldy #10    ; spend 55 cycles
    dey        ; before starting timer
    bne *-1
    stx $9115  ; start reference timer
pointers:
    sei
    lda #<irq
    sta $0314
    lda #>irq
    sta $0315
    lda #$c0
; enable timer A underflow interrupts
    sta $912e
    cli
    jmp *

    .org $1100
irq:
; irq (event)
; 7 + at least 2 cycles of last
; instruction (9 to 16 total)
; pha           ; 3
; txa           ; 2
; pha           ; 3
; tya           ; 2
; pha           ; 3
; tsx           ; 2
; lda $0104,x   ; 4
; and #xx       ; 2
; beq           ; 3
; jmp ($314)    ; 5
                ; ---
; 38 to 45 cycles delay at this stage
    lda $9114   ; get the NMI timer A

    cmp #$08    ; > 7 cycles ahead?
    bcc +
    pha         ; spend 8 extra cycles
    pla
    and #$07    ; and reset high bit
:   cmp #$04
    bcc +
    bit $24     ; waste 4 cycles
    and #$03
:   cmp #$02    ; spend rest of cycles
    bcs *+2
    bcs *+2
    lsr
    bcs *+2
    ; 82 cycles from start of IRQ

mainLoop
    lda plasmaCnt+0
    clc
    adc sineSpeeds+0
    sta plasmaCnt+0
    lda plasmaCnt+1
    clc
    adc sineSpeeds+1
    sta plasmaCnt+1

    lda add
    clc
    adc addSpeed
    and #$3f
    sta add

    lda #<SCREEN
    sta store
    lda #>SCREEN
    sta store+1

    lda #<COLORMEM
    sta store2
    lda #>COLORMEM
    sta store2+1

    lda #$00
    sta sine0
    sta sine1
    sta rtSine
    sta color
    sta char

    lda #HEIGHT-1
    sta yPos
yLoop
    ldx plasmaCnt+0
    ldy plasmaCnt+1
    clc
.eq sine0 *+1
    lda sine128,x
.eq sine1 *+1
    adc sine64,y
    sta lineOffset

    lda sine0
    clc
    adc #RTSPREAD0
    sta sine0
    lda sine1
    clc
    adc #RTSPREAD1
    sta sine1

    ldx #WIDTH-1
xLoop:
    lda sineOffsets,x
    clc
.eq lineOffset *+1
    adc #$00
    tay
    lda colorOffsets,x
    clc
.eq rtSine *+1
    adc sine64,y
    adc add
    tay
.eq char *+1
    lda charTable,y
.eq store *+1
    sta SCREEN,x
.eq color *+1
    lda colorTable,y
.eq store2 *+1
    sta COLORMEM,x
    dex
    bpl xLoop

    lda rtSine
    clc

    adc #SINSPREADY
    sta rtSine
    lda color
    clc
    adc #COLORSPREADY
    sta color

    lda store
    clc
    adc #SCREEN_W
    sta store
    sta store2
    bcc +
    inc store+1
    inc store2+1
:   dec yPos
    bpl yLoop
    jmp $eabf

;**************************************
; DATA
;
sineSpeeds .db $03, $fe
addSpeed   .db $ff

.org $1400

; charset
.db 0,0,0,0,0,0,0,0
.db 0,0,0,0,0,0,0,0
.db $55,$55,$55,$55,$55,$55,$55,$55
.db $55,$55,$55,$55,$55,$55,$55,$55
.db $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa
.db $aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa
.db $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
.db $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff

.db $11,$44,$11,$44,$11,$44,$11,$44
.db $11,$44,$11,$44,$11,$44,$11,$44
.db $88,$22,$88,$22,$88,$22,$88,$22
.db $88,$22,$88,$22,$88,$22,$88,$22
.db $cc,$33,$cc,$33,$cc,$33,$cc,$33
.db $cc,$33,$cc,$33,$cc,$33,$cc,$33

.db $99,$66,$99,$66,$99,$66,$99,$66
.db $99,$66,$99,$66,$99,$66,$99,$66
.db $dd,$77,$dd,$77,$dd,$77,$dd,$77
.db $dd,$77,$dd,$77,$dd,$77,$dd,$77

.db $bb,$ee,$bb,$ee,$bb,$ee,$bb,$ee
.db $bb,$ee,$bb,$ee,$bb,$ee,$bb,$ee

colors
.db $04.8
.db $04.8

.db $04.8
.db $04.8
.db $04.8
.db $04.8
.db $04.8

.db $04.8
.db $04.8
.db $04.8
.db $04.8
.db $03.8
.db $03.8
.db $03.8
.db $03.8
.db $03.8

chars:
.db 2  ; char (solid) - white
.db 9  ; char (grid)- white on aux
.db 3  ; aux (solid) - l blue
.db 8  ; aux (grid) l blue on cyan
.db 1  ; border (solid) cyan
.db 4  ; border (grid) cyan on l purple
.db 0  ; screen (solid) l purple
.db 5  ; char (grid) purple on l purple
.db 2  ; char (solid) purple
; reverse
.db 5  ; char (grid) purple on l purple
.db 0  ; screen (solid) l purple
.db 4  ; border (grid) cyan on l purple
.db 1  ; border (solid) cyan
.db 8  ; aux (grid) l blue on cyan
.db 3  ; aux (solid) - l blue
.db 9  ; char (grid)- white on aux
.db 1  ; char (solid) - white

    .org $1500
sine64:
.db $20
.db $20
.db $21
.db $22
.db $23
.db $23
.db $24
.db $25
.db $26
.db $27
.db $27
.db $28
.db $29
.db $2a
.db $2a
.db $2b
.db $2c
.db $2c
.db $2d
.db $2e
.db $2f
.db $2f
.db $30
.db $31
.db $31
.db $32
.db $33
.db $33
.db $34
.db $34
.db $35
.db $36
.db $36
.db $37
.db $37
.db $38
.db $38
.db $39
.db $39
.db $3a
.db $3a
.db $3b
.db $3b
.db $3b
.db $3c
.db $3c
.db $3c
.db $3d
.db $3d
.db $3d
.db $3e
.db $3e
.db $3e
.db $3e
.db $3f
.db $3f
.db $3f
.db $3f
.db $3f
.db $3f
.db $3f
.db $3f
.db $3f
.db $3f
.db $40
.db $3f
.db $3f
.db $3f
.db $3f
.db $3f
.db $3f
.db $3f
.db $3f
.db $3f
.db $3f
.db $3e
.db $3e
.db $3e
.db $3e
.db $3d
.db $3d
.db $3d
.db $3c
.db $3c
.db $3c
.db $3b
.db $3b
.db $3b
.db $3a
.db $3a
.db $39
.db $39
.db $38
.db $38
.db $37
.db $37
.db $36
.db $36
.db $35
.db $34
.db $34
.db $33
.db $33
.db $32
.db $31
.db $31
.db $30
.db $2f
.db $2f
.db $2e
.db $2d
.db $2c
.db $2c
.db $2b
.db $2a
.db $2a
.db $29
.db $28
.db $27
.db $27
.db $26
.db $25
.db $24
.db $23
.db $23
.db $22
.db $21
.db $20
.db $20
.db $1f
.db $1e
.db $1d
.db $1c
.db $1c
.db $1b
.db $1a
.db $19
.db $18
.db $18
.db $17
.db $16
.db $15
.db $15
.db $14
.db $13
.db $13
.db $12
.db $11
.db $10
.db $10
.db $f
.db $e
.db $e
.db $d
.db $c
.db $c
.db $b
.db $b
.db $a
.db $9
.db $9
.db $8
.db $8
.db $7
.db $7
.db $6
.db $6
.db $5
.db $5
.db $4
.db $4
.db $4
.db $3
.db $3
.db $3
.db $2
.db $2
.db $2
.db $1
.db $1
.db $1
.db $1
.db $0
.db $0
.db $0
.db $0
.db $0
.db $0
.db $0
.db $0
.db $0
.db $0
.db $0
.db $0
.db $0
.db $0
.db $0
.db $0
.db $0
.db $0
.db $0
.db $0
.db $0
.db $1
.db $1
.db $1
.db $1
.db $2
.db $2
.db $2
.db $3
.db $3
.db $3
.db $4
.db $4
.db $4
.db $5
.db $5
.db $6
.db $6
.db $7
.db $7
.db $8
.db $8
.db $9
.db $9
.db $a
.db $b
.db $b
.db $c
.db $c
.db $d
.db $e
.db $e
.db $f
.db $10
.db $10
.db $11
.db $12
.db $13
.db $13
.db $14
.db $15
.db $15
.db $16
.db $17
.db $18
.db $18
.db $19
.db $1a
.db $1b
.db $1c
.db $1c
.db $1d
.db $1e
.db $1f

.db $20
.db $20
.db $21
.db $22
.db $23
.db $23
.db $24
.db $25
.db $26
.db $27
.db $27
.db $28
.db $29
.db $2a
.db $2a
.db $2b
.db $2c
.db $2c
.db $2d
.db $2e
.db $2f
.db $2f
.db $30
.db $31
.db $31
.db $32
.db $33
.db $33
.db $34
.db $34
.db $35
.db $36
.db $36
.db $37
.db $37
.db $38
.db $38
.db $39
.db $39
.db $3a
.db $3a
.db $3b
.db $3b
.db $3b
.db $3c
.db $3c
.db $3c
.db $3d
.db $3d
.db $3d
.db $3e
.db $3e
.db $3e
.db $3e
.db $3f
.db $3f
.db $3f
.db $3f
.db $3f
.db $3f
.db $3f
.db $3f
.db $3f
.db $3f
.db $40
.db $3f
.db $3f
.db $3f
.db $3f
.db $3f
.db $3f
.db $3f
.db $3f
.db $3f
.db $3f
.db $3e
.db $3e
.db $3e
.db $3e
.db $3d
.db $3d
.db $3d
.db $3c
.db $3c
.db $3c
.db $3b
.db $3b
.db $3b
.db $3a
.db $3a
.db $39
.db $39
.db $38
.db $38
.db $37
.db $37
.db $36
.db $36
.db $35
.db $34
.db $34
.db $33
.db $33
.db $32
.db $31
.db $31
.db $30
.db $2f
.db $2f
.db $2e
.db $2d
.db $2c
.db $2c
.db $2b
.db $2a
.db $2a
.db $29
.db $28
.db $27
.db $27
.db $26
.db $25
.db $24
.db $23
.db $23
.db $22
.db $21
.db $20
.db $20
.db $1f

.db $1e
.db $1d
.db $1c
.db $1c
.db $1b
.db $1a
.db $19
.db $18
.db $18
.db $17
.db $16
.db $15
.db $15
.db $14
.db $13
.db $13
.db $12
.db $11
.db $10
.db $10
.db $f
.db $e
.db $e
.db $d
.db $c
.db $c
.db $b
.db $b
.db $a
.db $9
.db $9
.db $8
.db $8
.db $7
.db $7
.db $6
.db $6
.db $5
.db $5
.db $4
.db $4
.db $4
.db $3
.db $3
.db $3
.db $2
.db $2
.db $2
.db $1
.db $1
.db $1
.db $1
.db $0
.db $0
.db $0
.db $0
.db $0
.db $0
.db $0
.db $0
.db $0
.db $0
.db $0
.db $0
.db $0
.db $0
.db $0
.db $0
.db $0
.db $0
.db $0
.db $0
.db $0
.db $1
.db $1
.db $1
.db $1
.db $2
.db $2
.db $2
.db $3
.db $3
.db $3
.db $4
.db $4
.db $4
.db $5
.db $5
.db $6
.db $6
.db $7
.db $7
.db $8
.db $8
.db $9
.db $9
.db $a
.db $b
.db $b
.db $c
.db $c
.db $d
.db $e
.db $e
.db $f
.db $10
.db $10
.db $11
.db $12
.db $13
.db $13
.db $14
.db $15
.db $15
.db $16
.db $17
.db $18
.db $18
.db $19
.db $1a
.db $1b
.db $1c
.db $1c
.db $1d
.db $1e
.db $1f

sine128:
.db $40
.db $41
.db $43
.db $44
.db $46
.db $47
.db $49
.db $4a
.db $4c
.db $4e
.db $4f
.db $51
.db $52
.db $54
.db $55
.db $57
.db $58
.db $59
.db $5b
.db $5c
.db $5e
.db $5f
.db $60
.db $62
.db $63
.db $64
.db $66
.db $67
.db $68
.db $69
.db $6a
.db $6c
.db $6d
.db $6e
.db $6f
.db $70
.db $71
.db $72
.db $73
.db $74
.db $75
.db $76
.db $76
.db $77
.db $78
.db $79
.db $79
.db $7a
.db $7b
.db $7b
.db $7c
.db $7c
.db $7d
.db $7d
.db $7e
.db $7e
.db $7e
.db $7f
.db $7f
.db $7f
.db $7f
.db $7f
.db $7f
.db $7f
.db $80
.db $7f
.db $7f
.db $7f
.db $7f
.db $7f
.db $7f
.db $7f
.db $7e
.db $7e
.db $7e
.db $7d
.db $7d
.db $7c
.db $7c
.db $7b
.db $7b
.db $7a
.db $79
.db $79
.db $78
.db $77
.db $76
.db $76
.db $75
.db $74
.db $73
.db $72
.db $71
.db $70
.db $6f
.db $6e
.db $6d
.db $6c
.db $6a
.db $69
.db $68
.db $67
.db $66
.db $64
.db $63
.db $62
.db $60
.db $5f
.db $5e
.db $5c
.db $5b
.db $59
.db $58
.db $57
.db $55
.db $54
.db $52
.db $51
.db $4f
.db $4e
.db $4c
.db $4a
.db $49
.db $47
.db $46
.db $44
.db $43
.db $41
.db $40
.db $3e
.db $3c
.db $3b
.db $39
.db $38
.db $36
.db $35
.db $33
.db $31
.db $30
.db $2e
.db $2d
.db $2b
.db $2a
.db $28
.db $27
.db $26
.db $24
.db $23
.db $21
.db $20
.db $1f
.db $1d
.db $1c
.db $1b
.db $19
.db $18
.db $17
.db $16
.db $15
.db $13
.db $12
.db $11
.db $10
.db $f
.db $e
.db $d
.db $c
.db $b
.db $a
.db $9
.db $9
.db $8
.db $7
.db $6
.db $6
.db $5
.db $4
.db $4
.db $3
.db $3
.db $2
.db $2
.db $1
.db $1
.db $1
.db $0
.db $0
.db $0
.db $0
.db $0
.db $0
.db $0
.db $0
.db $0
.db $0
.db $0
.db $0
.db $0
.db $0
.db $0
.db $1
.db $1
.db $1
.db $2
.db $2
.db $3
.db $3
.db $4
.db $4
.db $5
.db $6
.db $6
.db $7
.db $8
.db $9
.db $9
.db $a
.db $b
.db $c
.db $d
.db $e
.db $f
.db $10
.db $11
.db $12
.db $13
.db $15
.db $16
.db $17
.db $18
.db $19
.db $1b
.db $1c
.db $1d
.db $1f
.db $20
.db $21
.db $23
.db $24
.db $26
.db $27
.db $28
.db $2a
.db $2b
.db $2d
.db $2e
.db $30
.db $31
.db $33
.db $35
.db $36
.db $38
.db $39
.db $3b
.db $3c
.db $3e

.db $40
.db $41
.db $43
.db $44
.db $46
.db $47
.db $49
.db $4a
.db $4c
.db $4e
.db $4f
.db $51
.db $52
.db $54
.db $55
.db $57
.db $58
.db $59
.db $5b
.db $5c
.db $5e
.db $5f
.db $60
.db $62
.db $63
.db $64
.db $66
.db $67
.db $68
.db $69
.db $6a
.db $6c
.db $6d
.db $6e
.db $6f
.db $70
.db $71
.db $72
.db $73
.db $74
.db $75
.db $76
.db $76
.db $77
.db $78
.db $79
.db $79
.db $7a
.db $7b
.db $7b
.db $7c
.db $7c
.db $7d
.db $7d
.db $7e
.db $7e
.db $7e
.db $7f
.db $7f
.db $7f
.db $7f
.db $7f
.db $7f
.db $7f
.db $80
.db $7f
.db $7f
.db $7f
.db $7f
.db $7f
.db $7f
.db $7f
.db $7e
.db $7e
.db $7e
.db $7d
.db $7d
.db $7c
.db $7c
.db $7b
.db $7b
.db $7a
.db $79
.db $79
.db $78
.db $77
.db $76
.db $76
.db $75
.db $74
.db $73
.db $72
.db $71
.db $70
.db $6f
.db $6e
.db $6d
.db $6c
.db $6a
.db $69
.db $68
.db $67
.db $66
.db $64
.db $63
.db $62
.db $60
.db $5f
.db $5e
.db $5c
.db $5b
.db $59
.db $58
.db $57
.db $55
.db $54
.db $52
.db $51
.db $4f
.db $4e
.db $4c
.db $4a
.db $49
.db $47
.db $46
.db $44
.db $43
.db $41
.db $40
.db $3e
.db $3c
.db $3b
.db $39
.db $38
.db $36
.db $35
.db $33
.db $31
.db $30
.db $2e
.db $2d
.db $2b
.db $2a
.db $28
.db $27
.db $26
.db $24
.db $23
.db $21
.db $20
.db $1f
.db $1d
.db $1c
.db $1b
.db $19
.db $18
.db $17
.db $16
.db $15
.db $13
.db $12
.db $11
.db $10
.db $f
.db $e
.db $d
.db $c
.db $b
.db $a
.db $9
.db $9
.db $8
.db $7
.db $6
.db $6
.db $5
.db $4
.db $4
.db $3
.db $3
.db $2
.db $2
.db $1
.db $1
.db $1
.db $0
.db $0
.db $0
.db $0
.db $0
.db $0
.db $0
.db $0
.db $0
.db $0
.db $0
.db $0
.db $0
.db $0
.db $0
.db $1
.db $1
.db $1
.db $2
.db $2
.db $3
.db $3
.db $4
.db $4
.db $5
.db $6
.db $6
.db $7
.db $8
.db $9
.db $9
.db $a
.db $b
.db $c
.db $d
.db $e
.db $f
.db $10
.db $11
.db $12
.db $13
.db $15
.db $16
.db $17
.db $18
.db $19
.db $1b
.db $1c
.db $1d
.db $1f
.db $20
.db $21
.db $23
.db $24
.db $26
.db $27
.db $28
.db $2a
.db $2b
.db $2d
.db $2e
.db $30
.db $31
.db $33
.db $35
.db $36
.db $38
.db $39
.db $3b
.db $3c
.db $3e

sineOffsets
.db 0
.db 3
.db 6
.db 9
.db 12
.db 15
.db 18
.db 21
.db 24
.db 27
.db 30
.db 33
.db 36
.db 39
.db 42
.db 45
.db 48
.db 51
.db 54
.db 57
.db 60
.db 63
.db 66
.db 69
.db 72
.db 75
.db 78
.db 81
.db 84
.db 87
.db 90
.db 93
.db 96
.db 99
.db 102
.db 105
.db 108
.db 111
.db 114
.db 117
colorOffsets
.db 0
.db 3
.db 6
.db 9
.db 12
.db 15
.db 18
.db 21
.db 24
.db 27
.db 30
.db 33
.db 36
.db 39
.db 42
.db 45
.db 48
.db 51
.db 54
.db 57
.db 60
.db 63
.db 66
.db 69
.db 72
.db 75
.db 78
.db 81
.db 84
.db 87
.db 90
.db 93
.db 96
.db 99
.db 102
.db 105
.db 108
.db 111
.db 114
.db 117

.eq colorTable $1a00
.eq charTable $1c00
