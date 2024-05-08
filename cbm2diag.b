; cbm2 diagnostic-cart 324835-01
; disassembled by Vossi 05/2024, fixed/enhanced 05/2024
; assemble with ACME
!cpu 6502
!ct scr		; standard text/char conversion table -> Screencode (pet = PETSCII, raw)
!to "cbm2diag.bin", plain
; * switches
STACKINITFIX	= 1	; Fixes stack init 
STATICFULL	= 1	; Tests full 2kB static RAM
; ***************************************** CONSTANTS *********************************************
FILL			= $ff		; fills free memory areas with $ff
SYSTEMBANK		= $0f		; systembank
; test parameters
TIMERCYCLES		= $0a70		; compare cycles (determined with a LA and a real P500)
; VDC register
ADR			= $0		; address register
DATA			= $1		; data register
; TPI register
PA			= $0		; port a
PB			= $1		; port b
PC			= $2		; port c
LIR			= $2		; interrupt latch register
DDPA			= $3		; data direction port a
DDPB			= $4		; data direction port b
DDPC			= $5		; data direction port c
MIR			= $5		; interrupt mask register
CR			= $6		; control register
AIR			= $7		; active interrupt register
; CIA register
PRA			= $0		; port a
PRB			= $1		; port b
DDRA			= $2		; direction port a
DDRB			= $3		; direction port b
TALO			= $4		; timer a lo
TAHI			= $5		; timer a hi
TBLO			= $6		; timer b lo
TBHI			= $7		; timer b hi
TOD10			= $8		; tod 10th of seconds
TODSEC			= $9		; tod seconds
TODMIN			= $a		; tod monutes
TODHR			= $b		; tod hours
SDR			= $c		; serial data register
ICR			= $d		; interrupt control register
CRA			= $e		; control register b
CRB			= $f		; control register b
; ACIA register
DRSN			= $0		; data register
SRSN			= $1		; status register
CDR			= $2		; command register
CTR			= $3		; control register
; SID register
OSC1			= $00		; oscillator 1
OSC2			= $07		; oscillator 2
OSC3			= $0e		; oscillator 3
FREQLO			= $00		; frequency lo
FREQHI			= $01		; frequency hi
PWLO			= $02		; pulse width lo
PWHI			= $03		; pulse width hi
OSCCTL			= $04		; oscillator control
ATKDCY			= $05		; attack/decay
SUSREL			= $06		; sustain/release
FCLOW			= $15		; filter low
FCHI			= $16		; filter high
RESFILT			= $17		; resonance/filter
VOLUME			= $18		; volume
; ***************************************** ADDRESSES *********************************************
!addr CodeBank		= $00		; code bank register
!addr IndirectBank	= $01		; indirect bank register
!addr MemZero		= $0000
!addr databits		= $0010		; databits dram test bad
!addr tempirq		= $0104		; temp irq handler
!addr cinv		= $300		; irq vector
!addr cbinv		= $302		; irq break vector
!addr nminv		= $304		; nmi vector
!addr ScreenRAM		= $d000		; Screen RAM
!addr vdc		= $d800		; VDC
!addr sid		= $da00		; SID
!addr cia		= $dc00		; CIA
!addr acia		= $dd00		; ACIA
!addr tpi1		= $de00		; TPI1
!addr tpi2		= $df00		; TPI2
; ***************************************** ZERO PAGE *********************************************
!addr RamEnd		= $02		; last dram bank +1
!addr TestBank		= $03		; dram test bank
!addr machinetype	= $04		; machine type: $40=LP, $80=HP
!addr temp1		= $05		; temp
!addr temp5		= $06		; temp irq sub
!addr temp4		= $07		; temp irq sub
!addr temp2		= $08		; temp
!addr temp3		= $09		; temp
!addr pointer1		= $0a		; 16bit pointer
!addr romsize		= $0c		; rom size in pages
!addr unused		= $28		; never used
!addr pointer_screen	= $2a		; 16bit pointer screen text position
!addr pointer_text	= $2c		; 16bit pointer text
!addr pointer_screen2	= $2e		; 16bit pointer screen text position
; ***************************************** ZONE MAIN *********************************************
!zone main
!initmem FILL
*= $2000
	jmp Start			; jump to start
	jmp Start			; jump to start
	!byte $43, $c2, $cd, "2"	; cbm-rom ident-bytes 'C'= without init, 'BM', '2' = 4k-block 2
Start:	sei
	lda #SYSTEMBANK
	sta IndirectBank
!ifdef STACKINITFIX{
	ldx #$ff			; fix stack init 		******** PATCHED ********
} else{
	ldx $ff				; ************ BUG ???????????????
}
	txs				; reset stack pointer
	cld
	ldy #$11			; 18 VDC register
	lda tpi2+PC			; check machine type bit#7
	bmi hpvdclp			; -> HP machine
lpvdclp:lda VDCInitValuesLP,y		; init LP VDC register
	sty vdc+ADR
	sta vdc+DATA
	dey
	bpl lpvdclp
	lda #$40			; type=LP
	jmp Init
; ----------------------------------------------------------------------------
; LP vdc init values
VDCInitValuesLP: 
	!byte $7f, $50, $60, $0a, $26, $01, $19, $1e
	!byte $00, $07, $20, $00, $00, $00, $00, $00
	!byte $00, $00
; ----------------------------------------------------------------------------
; 
hpvdclp:lda VDCInitValuesHP,y
	sty vdc+ADR
	sta vdc+DATA
	dey
	bpl hpvdclp
	lda #$80			; type=HP
	jmp Init
; ----------------------------------------------------------------------------
; HP vdc init values
VDCInitValuesHP:
	!byte $6c, $50, $53, $0f, $19, $03, $19, $19
	!byte $00, $0d, $20, $00, $00, $00, $00, $00
	!byte $00, $00
; ----------------------------------------------------------------------------
; 
Init:	sta machinetype			; store machine type
; clear screen
	ldx #$00
	lda #' '
clrlp:  sta ScreenRAM,x
	sta ScreenRAM+$100,x
	sta ScreenRAM+$200,x
	sta ScreenRAM+$300,x
	sta ScreenRAM+$400,x
	sta ScreenRAM+$500,x
	sta ScreenRAM+$600,x
	sta ScreenRAM+$700,x
	inx
	bne clrlp
; check ram banks
	ldx #$03			; minimum 2 banks / 128kB: testbank = 3
	stx RamEnd			; store last ram bank +1
	stx IndirectBank		; indirect bank=3
	ldx #$24			; title text length
	lda #$60			; test at address $xx60
	sta temp2
	lda #$a5			; test value
	sta temp1
ckramlp:sta (temp2),y			; y already 0
	lda (temp2),y
	cmp temp1			; check testbyte ?
	beq bank3pr			; -> test ok
	iny
	bne ckramlp
	beq ram128
bank3pr:lda #$05			; 4 banks / 256kB
	sta RamEnd			; store last ram bank +1
	bne ram256			; jump always
ram128: lda machinetype			; check lp/hp
	bmi hp128			; -> hp
; print title
lp128:	lda TitleLP128,x		; title 600 128k
	and #$bf
	sta ScreenRAM,x
	dex
	bne lp128
	beq Cycles			; always
;
hp128:	lda TitleHP128,x		; title 700 128k
	and #$bf
	sta ScreenRAM,x
	dex
	bne hp128
	beq Cycles			; always
;
ram256: lda machinetype			; check lp/hp
	bmi hp256			; -> hp
lp256:	lda TitleLP256,x		; title 600 256k
	and #$bf
	sta ScreenRAM,x
	dex
	bne lp256
	beq Cycles			; always
;
hp256:	lda TitleHP256,x		; title 700 256k
	and #$bf
	sta ScreenRAM,x
	dex
	bne hp256
; print cycles
Cycles:	lda #SYSTEMBANK
	sta IndirectBank		; indirect = systembank
	ldx #$08			; text length
prcyclp:lda TextCycles,x		; print "CYCLES"
	and #$bf
	sta ScreenRAM+1*80,x
	lda Text000001,x		; print "000001"
	and #$bf
	ora #$80			; inverse
	sta ScreenRAM+1*80+16,x
	dex
	bne prcyclp
; test zeropage
TestZeropage:
	ldx #$10			; text length
przplp:	lda TextZeropage,x		; print "ZEROPAGE"
	and #$bf
	sta ScreenRAM+3*80,x
	dex
	bne przplp
	ldy #$03			; start at $0003
zplp:	ldx #$00
zpcntlp:txa
	sta MemZero,y			; zeropage
	eor MemZero,y
	bne zpbad			; -> bad
	inx 
	bne zpcntlp			; count byte up
	tya
	sta MemZero,y			; store address for check
	iny
	bne zplp			; next byte
	ldy #$03			; start at $0003
zpchklp:tya
	cmp MemZero,y			; check stored address values
	bne zpbad
	iny
	bne zpchklp
	ldx #$03
zpoklp:	lda TextOK,x			; zeropage ok
	and #$bf
	sta ScreenRAM+3*80+16,x
	dex
	bne zpoklp
	jmp TestStaticRam
zpbad:	ldx #$03
zpbadlp:lda TextBad,x			; zeropage bad
	and #$bf
	ora #$80
	sta ScreenRAM+3*80+16,x
	dex
	bne zpbadlp
endless:lda #$01			; shift all bits in faulty byte
zpshft:	sta MemZero,y
	eor #$ff
	sta MemZero,y
	asl
	bcc zpshft
	jmp endless
; test static RAM
TestStaticRam:
	ldx #$10			; text length
prstalp:lda TextStaticRam,x		; print "STATIC RAM"
	and #$bf
	sta ScreenRAM+4*80,x
	dex
	bne prstalp
	ldx #$00
	stx pointer1
	inx				; start at $0100
	stx pointer1+1
	ldy #$00
statlp:	ldx #$00
statclp:txa
	sta (pointer1),y
	eor (pointer1),y
	bne statbad			; -> bad
	inx
	bne statclp			; count byte up
	tya
	clc
	adc pointer1+1			; add highbyte
	sta (pointer1),y		; store address for check
	iny
	bne statlp			; next byte
	inc pointer1+1			; inc page
	lda pointer1+1
!ifdef STATICFULL{
	cmp #$08			; test full 2kB static RAM		******** PATCHED ********
} else{
	cmp #$04			; original tests only 4 pages = 1kB
}
	bne statlp			; next page
	lda #$00
	sta pointer1
	ldx #$01			; start at $0100
	stx pointer1+1
	ldy #$00
stacklp:  tya
	clc
	adc pointer1+1
	cmp (pointer1),y		; check stored address values
	bne statbad			; -> bad
	iny
	bne stacklp			; next byte
	inx
	stx pointer1+1
!ifdef STATICFULL{
	cpx #$08			; check full 2kB static RAM		******** PATCHED ********
} else{
	cpx #$04			; original checks only 4 pages = 1kB
}
	bne stacklp			; next page
	ldx #$03
staoklp:lda TextOK,x			; static ram ok
	and #$bf
	sta ScreenRAM+4*80+16,x
	dex
	bne staoklp
	jmp MainTest
statbad:ldx #$03
stbadlp:lda TextBad,x			; static ram bad
	and #$bf
	ora #$80			; inverse
	sta ScreenRAM+4*80+16,x
	dex
	bne stbadlp
stashft:lda #$01			; shift all bits in faulty byte
	sta (pointer1),y
	eor #$ff
	sta (pointer1),y
	asl
	bcc stashft
	jmp MainTest
MainTest:
	nop
	nop
	nop
	lda #<(ScreenRAM+5*80)		; screen position for next text
	sta pointer_screen
	lda #>(ScreenRAM+5*80)
	sta pointer_screen+1
	lda #$10
	sta unused			; never used
	jsr TestVideoRam
	jsr TestRoms
	jsr TestKeyboard
	jsr TestRS232
	jsr TestCassette
	jsr TestUserPort
	jsr TestIeeePort
	jsr TestTimers
	jsr TestInterrupt
	jsr TestDram
	jsr TestSoundchip
	lda #5
	jsr Delay			; delay sub 5x
	jsr IncCounterClearScreen
	jmp TestZeropage
; ----------------------------------------------------------------------------
; test video ram
TestVideoRam:
	ldx #>TextVideoRam
	ldy #<TextVideoRam
	jsr PrintText			; print "video ram"
	ldy #<ScreenRAM			; set start address
	sty pointer1
	lda #>ScreenRAM
	sta pointer1+1
vidlp:  lda (pointer1),y		; load old value
	sta temp2			; remember
	ldx #$00
vidcnlp:txa
	sta (pointer1),y
	eor (pointer1),y
	bne vidbad			; -> bad
	inx
	bne vidcnlp			; count byte up
	lda temp2
	sta (pointer1),y		; restore byte
	iny
	bne vidlp			; next byte
	inc pointer1+1			; inc page
	lda pointer1+1
	cmp #>(ScreenRAM+$800)		; video ram end ?
	bne vidlp			; next page
	jsr PrintOK
	jsr AddLine
	rts
; video ram bad
vidbad: jsr PrintBad
videndl:clc				; count faulty screen byte up
	adc #$01
	sta (pointer1),y
	jmp videndl			; endless
; ----------------------------------------------------------------------------
; ROM test tables
RomStartHigh:	!byte $80, $a0, $e0
RomSizePages:	!byte $20, $20, $20
RomChecksums:	!byte $80, $a0, $e0
; ----------------------------------------------------------------------------
; test ROM check sums
TestRoms:
	ldx #>TextBasicRomL
	ldy #<TextBasicRomL
	jsr PrintText			; print "basic rom l"
	ldx #$00			; Basic ROM low
	jsr TestRomx
	ldx #>TextBasicRomH
	ldy #<TextBasicRomH
	jsr PrintText			; print "basic rom h"
	ldx #$01			; Basic ROM high
	jsr TestRomx
	ldx #>TextKernalRom
	ldy #<TextKernalRom
	jsr PrintText			; print "kernal rom"
	ldx #$02			; Kernal ROM
	jsr TestRomx
	rts
; ----------------------------------------------------------------------------
; test rom x from table
TestRomx:
	ldy #$00
	sty pointer1
	lda RomStartHigh,x		; ROM start address high from table
	sta pointer1+1
	lda RomSizePages,x		; ROM size in pages
	sta romsize
	clc
	lda #$00
romlp:  adc (pointer1),y		; add rom byte with carry
	iny
	bne romlp			; add next byte
	inc pointer1+1
	dec romsize
	bne romlp			; next page
	adc #$00			; add last carry
	sta temp2			; store checksum
	cmp RomChecksums,x		; compare with table ?
	bne rombad			; -> not ok
	jsr PrintOK			; print ok
	jsr AddLine
	rts
; rom checksum bad
rombad:	jsr PrintBad			; print bad
	lda #24
	jsr AddChars			; add 24 chars
	lda temp2
	jsr PrintByteHex		; print wrong checksum
	jsr AddLine
	rts
; ----------------------------------------------------------------------------
; test keyboard / tpi2
TestKeyboard:
	ldx #>TextKeypoard
	ldy #<TextKeypoard
	jsr PrintText			; print "keyboard"
	ldy #$00
	sty tpi2+DDPA			; all tpi2 ports input
	sty tpi2+DDPB
	sty tpi2+DDPC
	ldx #$01
kportlp:lda #$3f
	sta tpi2+DDPA,x			; define bit 0-5 outputs			
	ldy #$c0
kbitlp:	tya
	sta tpi2+PA,x			; ouput data
	sta temp3			; remember value
	lda tpi2+PC
	eor temp3			; check inputs
	and #$3f			; isolate bit 0-5
	bne keybad			; -> keyboard bad
	iny
	bne kbitlp			; next bit
	lda #$00
	sta tpi2+DDPA,x			; port input
	dex
	bpl kportlp			; next port
	lda #$c0
	sta tpi2+PA			; set bit 6+7
	sta tpi2+PB			; set bit 6+7
	sta tpi2+DDPA			; define bit 6-7 outputs
	sta tpi2+DDPB			; define bit 6-7 outputs
	lda #$30
	sta temp3			; test start value for bit 6+7
	ldy #$07
k67btlp:ldx #$01
k67ptlp:lda temp3
	ora #$3f
	sta tpi2+PA,x			; set bits
	lda tpi2+PC
	and #$0f			; isolate high nibble
	cmp KeyboardBit67Values,y	; check with table
	bne keybad			; -> keyboard bad
	dey
	dex
	beq k67ptlp			; next port
	asl temp3			; shift bit
	cpy #$ff
	bne k67btlp			; next bit
	jsr PrintOK			; print ok
keyend:	jsr AddLine
	rts
; keyboard bad
keybad:	jsr PrintBad
	jmp keyend
; ----------------------------------------------------------------------------
; table keyboard bit 6+7 values
KeyboardBit67Values:	!byte $0a, $0b, $0f, $0d, $05, $04, $00, $03
; ----------------------------------------------------------------------------
; test RS232 / acia
TestRS232:
	ldx #>TextRS232
	ldy #<TextRS232
	jsr PrintText			; print "rs 232"
	sta acia+SRSN			; $20 (space from PrintText)
	lda acia+CTR
	ora #$10			; clock source = baud rate generator
	sta acia+CTR
	lda acia+CDR
	ora #$19			; mode=normal, transmit irq=off, RTS-level =low, enable/DTR=low
	sta acia+CDR
	lda #$0e			; baud rate 9600 / test byte count
	sta temp2
rs232lp:lda acia+CTR
	and #$f0			; clear baud rate bits
	ora temp2			; set baud rate
	sta acia+CTR
	ldy temp2
	lda TestBytesRS232,y		; load test byte from table
	sta acia+DRSN			; send
	lda acia+CDR
	and #$f7			; enable transmit interrupt
	sta acia+CDR
rsreclp:lda acia+SRSN			; load status
	and #$08			; isolate transmit data register full bit
	beq rsreclp			; wait for full transmit
	lda acia+DRSN
	cmp TestBytesRS232,y		; check received data
	bne aciabad			; acia bad
	dec temp2
	bne rs232lp			; next test byte
	lda acia+CDR
	and #$fe			; disable transmit/receive / DTR=high
	sta acia+CDR
	lda acia+SRSN
	tax
	and #$20			; isolate DCD
	beq aciabad			; DCD=0 -> acia bad
	txa
	and #$40			; isolate DSR
	beq aciabad			; DSR=0 -> acia bad
	sta acia+SRSN
	jsr PrintOK			; print ok
endacia:jsr AddLine
	rts
; RS232 bad
aciabad:jsr PrintBad			; print bad
	jmp endacia
; ----------------------------------------------------------------------------
; rs232 test table
TestBytesRS232:	!byte $ff, $55, $aa, $00, $01, $02, $04, $08
		!byte $10, $20, $40, $80, $ff, $cc, $33, $ff
; ----------------------------------------------------------------------------
; test cassette port
TestCassette:
	ldx #>TextCassette
	ldy #<TextCassette
	jsr PrintText			; print "cassette"
	lda tpi1+DDPB
	and #$7f
	ora #$60
	sta tpi1+DDPB
	ldx #$10
	stx cia+ICR
	ldx cia+ICR
	ldx #$04
	lda tpi1+PB
caslp:  ora #$60
	sta tpi1+PB
	and #$df
	pha
	pla
	sta tpi1+PB
	pha
	pla
	dex
	bne caslp
	lda #$f5
caslp2:	adc #$01
	bne caslp2
	lda tpi1+PB
	and #$80
	bne casbad
	lda cia+ICR
	and #$10
	beq casbad
	lda tpi1+PB
	and #$bf
	sta tpi1+PB
	lda #$f5
caslp3:	adc #$01
	bne caslp3
	lda tpi1+PB
	and #$80
	beq casbad
	lda tpi1+DDPB
	and #$df
	sta tpi1+DDPB
	jsr PrintOK			; print ok
casend:	jsr AddLine
	rts
; cassette bad
casbad:	jsr PrintBad			; print bad
	jmp casend
; ----------------------------------------------------------------------------
; test user port
TestUserPort:
	ldx #>TextUserPort
	ldy #<TextUserPort
	jsr PrintText			; print "user port"
	lda #$ff
	sta tpi1+PA			; all ports output high
	sta tpi1+PB
	sta tpi1+DDPB
	sta tpi1+DDPA
	lda #$cc
	sta cia+DDRA
	sta cia+DDRB
	eor #$ff
	sta temp2
	jsr usersub
	bcs userbad
	lda #$33
	sta cia+DDRA
	sta cia+DDRB
	eor #$ff
	sta temp2
	jsr usersub
	bcs userbad
	ldy cia+PRB
	nop
	lda cia+ICR
	and #$10
	beq userbad
	lda cia+CRA
	and #$bf
	sta cia+CRA
	ldx #$10
	clc
userlp:	lda #$04
	adc temp2
	sta temp2
	ora #$10
	sta tpi1+PB
	dex
	bne userlp
	lda cia+ICR
	and #$08
	beq userbad
	lda cia+SDR
	cmp #$55
	bne userbad
	jsr PrintOK			; print ok
userend:jsr AddLine
	lda #$ff
	sta tpi1+DDPA
	sta cia+DDRA
	sta cia+DDRB
	rts
; user port sub
usersub:ldy #$0f
userlp2:ldx #$01
userlp3:lda TestTable05a,y
	sta cia+PRA,x
	nop
	lda cia+PRA,x
	and temp2
	sta temp3
	lda TestTable05a,y
	and temp2
	cmp temp3
	bne usrsubx
	dex
	bpl userlp3
	dey
	bpl userlp2
	clc
	rts
usrsubx:sec
	rts
; user port bad
userbad:jsr PrintBad			; print bad
	jmp userend
; ----------------------------------------------------------------------------
; user port test tables
TestTable05a:	!byte $00, $05, $0a

TestBytesUser:	!byte $0f, $50, $55, $5a, $5f, $a0, $a5, $aa
		!byte $af, $f0, $f5, $fa, $ff
; ----------------------------------------------------------------------------
; test ieee port
TestIeeePort:
	ldx #>TextIeeePort
	ldy #<TextIeeePort
	jsr PrintText			; print "ieee port"
	lda #$ff
	sta cia+DDRA
	lda #$33
	sta tpi1+DDPA
	lda tpi1+DDPB
	and #$fe
	ora #$02
	sta tpi1+DDPB
	ldy #$0f
ieeelp:	lda TestTable05a,y
	sta cia+PRA
	lda tpi1+PA
	and #$cc
	cmp TestBytesIeee,y
	bne ieeebad
	dey
	bpl ieeelp
	ldx #$fe
	stx cia+PRA
	lda tpi1+PB
	and #$01
	bne ieeebad
	inx
	stx cia+PRA
	lda tpi1+PB
	and #$01
	beq ieeebad
	lda cia+PRA
	and #$f7
	sta cia+PRA
	lda tpi1+DDPA
	and #$df
	ora #$10
	sta tpi1+DDPA
	lda tpi1+PA
	and #$ef
	sta tpi1+PA
	lda tpi1+PA
	and #$20
	bne ieeebad
	lda tpi1+PA
	ora #$10
	sta tpi1+PA
	lda tpi1+PA
	and #$20
	beq ieeebad
	lda #$00
	sta tpi1+DDPA
	sta tpi1+DDPB
	sta cia+DDRA
	sta cia+DDRB
	jsr PrintOK			; print ok
ieeeend:jsr AddLine
	rts
; ieee bad
ieeebad:jsr PrintBad			; print bad
	jmp ieeeend
; ----------------------------------------------------------------------------
; ieee test table
TestBytesIeee:	!byte $00, $04, $08, $0c, $40, $44, $48, $4c
		!byte $80, $84, $88, $8c, $c0, $c4, $c8, $cc
; ----------------------------------------------------------------------------
; test cia timers
TestTimers:
	ldx #>TextTimers
	ldy #<TextTimers
	jsr PrintText			; print "timers"
	lda #$80
	sta cia+CRA
	sta cia+CRB
	lda #$ff
	sta cia+TAHI
	sta cia+TALO
	sta cia+TBHI
	sta cia+TBLO
	lda #$81
	sta cia+CRA
	sta cia+CRB
	lda #4
	jsr Delay			; delay sub 4x
	lda #$80
	sta cia+CRA
	sta cia+CRB
	lda cia+TAHI
	cmp #$ff
	beq tmrbad
	lda cia+TBHI
	cmp #$ff
	beq tmrbad
	cmp cia+TAHI
	bne tmrbad
	lda cia+TALO
	cmp #$ff
	beq tmrbad
	lda cia+TBLO
	cmp #$ff
	beq tmrbad
	sec
	sbc cia+TALO
	bcs tmrskp
	adc #$04
tmrskp:	cmp #$02
	bcs tmrbad
	jsr PrintOK			; print ok
tmrend:	jsr AddLine
	rts
; timers bad
tmrbad:	jsr PrintBad			; print bad
	jmp tmrend
; ----------------------------------------------------------------------------
; test interrupt
TestInterrupt:
	ldx #>TextInterrupt
	ldy #<TextInterrupt
	jsr PrintText			; print "interrupt"
	lda #<IRQHandler
	ldx #>IRQHandler
	stx cinv+1			; set interrrupt handler addresse
	stx cbinv+1
	sta cinv
	sta cbinv
	lda #<IRQHandler2
	ldx #>IRQHandler2
	stx nminv+1			; set nmi handler addresse
	sta nminv
	lda tpi1+CR
	and #$fd
	ora #$31
	sta tpi1+CR
	lda tpi1+DDPC
	ora #$04
	sta tpi1+DDPC
	lda #$01
	sta temp4
	lda #$0e
	sta temp2
	ldy #$04
	jsr irqsub2
	ldy #$06
	asl temp4
	inc temp2
	jsr irqsub2
	asl temp4
	lda #$00
	sta cia+CRB
	ldx #$01
	jsr irqsub1
	lda #$80
	sta cia+CRB
	ldx #$04
	jsr irqsub1
	jsr irqsub3
	lda #2
	jsr Delay			; delay sub 2x
	jsr irqsub4
	jsr PrintOK			; print ok
	jsr AddLine
	rts
; irq sub1
irqsub1:lda #$00
	sta cia+TODHR
	sta cia+TODMIN
	sta cia+TODSEC
	stx cia+TOD10
	rts
; irq sub2
irqsub2:lda #$ff
	sta cia+PRA,y
	iny
	lda #$02
	sta cia+PRA,y
	jsr irqsub3
	ldy temp2
	lda #$89
	sta cia+PRA,y
	jsr irqsub4
	rts
; irq sub3
irqsub3:lda temp4
	ora #$80
	sta cia+ICR
	lda #$00
	sta temp5
	cli
	rts
; irq sub4
irqsub4:lda cia+ICR
	and temp4
	beq irqsub4
	lda #$00
	sta cia+ICR
	sei
	lda temp5
	beq irqbad
	clc
	rts
; interrupt bad
irqbad:	pla
	pla
	jsr PrintBad			; print bad
	jsr AddLine
	sec
	rts
; ----------------------------------------------------------------------------
; irq test handler
IRQHandler:
	lda tpi1+AIR
	dec temp5
	tsx
	lda tempirq,x
	ora #$04
	sta tempirq,x
	pla
	tay
	pla
	tax
	pla
	rti
IRQHandler2:
	dec temp5
	rti
; ----------------------------------------------------------------------------
; test dram segments (banks)
TestDram:
	lda #$01			; start with bank 1
	sta TestBank
banklp:	ldx #>TextDram
	ldy #<TextDram
	jsr PrintText			; print "dram segment"
	ldy TestBank
	lda HexScreenCode,y		; load bank screencode
	and #$bf
	ldy #$0e
	sta (pointer_screen),y		; print bank number
	jsr testbnk
	inc TestBank
	lda TestBank
	cmp RamEnd			; reach last ram bank ?
	bne banklp			; next bank
	rts
; test dram bank
testbnk:lda #$00
	sta pointer1			; set start to $0000
	sta pointer1+1
	lda TestBank
	sta IndirectBank		; set indirect bank to testbank
	ldy #$02			; start with $0002
dramlp:	lda #$55			; test with $55
	sta (pointer1),y
	lda (pointer1),y
	eor #$55
	bne da5bad			; -> bad
	lda #$aa			; test with $aa
	sta (pointer1),y
	lda (pointer1),y
	eor #$aa
	bne da5bad			; -> bad
	tya
	clc
	adc pointer1+1
	sta (pointer1),y		; store address+high
dramct1:iny
	bne dramlp			; next byte
	inc pointer1+1
	bne dramlp			; next page
	lda #$00			; start to $0002
	sta pointer1+1
	ldy #$02
dramlp2:tya
	clc
	adc pointer1+1
	sta temp2
	lda (pointer1),y		; check stored addresses
	eor temp2
	bne dadrbad			; -> bad
dramct2:iny
	bne dramlp2			; next byte
	inc pointer1+1
	bne dramlp2			; next page
	lda #$0f
	sta IndirectBank		; systembank
	jsr PrintOK			; print bank ok
	jsr AddLine
	rts
; 
da5bad:	jsr drambad
	beq dramct1			; always continue
dadrbad:jsr drambad
	beq dramct2			; always continue
drambad:sty pointer1
	sta temp1
	lda #$0f
	sta IndirectBank		; systembank
	jsr PrintDatabits
	jsr PrintAddress
	ldy pointer1
	lda TestBank
	sta IndirectBank		; indirect = testbank
	lda #$00
	sta pointer1
	rts
; ----------------------------------------------------------------------------
; not used
	lda TestBank
	sta IndirectBank
	ldy #$00
-	clc
	adc #$01
	sta (pointer1),y
	jmp -
; ----------------------------------------------------------------------------
; test soundchip
TestSoundchip:
	ldx #>TextSoundchip
	ldy #<TextSoundchip
	jsr PrintText
	ldy #$14
soundlp:lda TestBytesSound,y
	sta sid+OSC1+FREQLO,y
	dey
	bpl soundlp
	lda #$0f
	sta sid+VOLUME
	lda #$11
	jsr sndsub
	lda #$21
	jsr sndsub
	lda #$41
	jsr sndsub
	lda #$01
	sta sid+RESFILT
	lda #$2f
	sta sid+VOLUME
	lda #$00
	sta sid+OSC1+ATKDCY
	lda #$f0
	sta sid+OSC1+SUSREL
	lda #$81
	sta sid+OSC1+OSCCTL
	ldx #$00
sndlp2:	ldy #$00
sndlp3:	sty sid+FCLOW
	pha
	pla
	pha
	pla
	pha
	pla
	iny
	cpy #$80
	bne sndlp3
	stx sid+FCHI
	inx
	bne sndlp2
	stx sid+VOLUME
	jsr AddLine
	rts
; sound test sub
sndsub:	sta sid+OSC1+OSCCTL
	sta sid+OSC1+SUSREL
	sta sid+OSC3+OSCCTL
	lda #1
	jsr Delay			; delay sub 1x
	lda #$00
	sta sid+OSC1+OSCCTL
	sta sid+OSC1+SUSREL
	sta sid+OSC3+OSCCTL
	rts
; ----------------------------------------------------------------------------
; sound test table
TestBytesSound:	!byte $1c, $d6, $ff, $00, $10, $09, $00, $24
		!byte $55, $ff, $00, $10, $09, $00, $2b
; ----------------------------------------------------------------------------
; not used
		!byte $34, $ff, $00, $10, $09, $00
; ----------------------------------------------------------------------------
; print text 16 chars
PrintText:
	stx pointer_text+1				; store text address to pointer
	sty pointer_text
	ldy #$10
prtlp:	lda (pointer_text),y
	and #$bf
	sta (pointer_screen),y
	dey
	bpl prtlp
	rts
; ----------------------------------------------------------------------------
;  print text with length in a
PrintTexta:
	stx pointer_text+1
	sty pointer_text
	tay
prtxalp:lda (pointer_text),y
	and #$bf
	sta (pointer_screen2),y
	dey
	bpl prtxalp
	rts
; ----------------------------------------------------------------------------
; print ok
PrintOK:
	lda #$10
	jsr AddChars			; add 16 chars for ok
	ldy #$03
proklp:	lda TextOK,y
	and #$bf
	sta (pointer_screen2),y
	dey
	bne proklp
	rts
; ----------------------------------------------------------------------------
; print bad
PrintBad:
	lda #$10
	jsr AddChars			; add 16 chars for bad
	ldy #$03
prbadlp:lda TextBad,y
	and #$bf
	ora #$80			; inverse
	sta (pointer_screen2),y
	dey
	bne prbadlp
	rts
; ----------------------------------------------------------------------------
; print dram test bad databits
PrintDatabits:
	jsr StoreDatabits		; store databits to $0010
	jsr PrintBad			; print bad
	lda #24
	jsr AddChars			; add 24 chars
	ldx #>TextDatabits
	ldy #<TextDatabits
	lda #$08
	jsr PrintTexta			; print "databits"
	lda #35
	jsr AddChars			; add 35 chars
	ldx #>databits
	ldy #<databits
	lda #$07			; 8 bits
	jsr PrintTexta			; print databits
	rts
; ----------------------------------------------------------------------------
; add line to screen pointer
AddLine:
	clc
	lda pointer_screen
	adc #80
	sta pointer_screen
	lda pointer_screen+1
	adc #$00
	sta pointer_screen+1
	rts
; ----------------------------------------------------------------------------
;  add a chars to screen pointer and save to screen pointer2
AddChars:
	clc
	adc pointer_screen
	sta pointer_screen2
	lda pointer_screen+1
	adc #$00
	sta pointer_screen2+1
	rts
; ----------------------------------------------------------------------------
; store databits to $0010
StoreDatabits:
	ldy #$07			; 8 bits
	ldx temp1			; load faulty byte
dbitlp:	txa
	lsr
	tax
	bcs dbithi
	lda HexScreenCode		; "0"
	bcc dbitlo			; always
dbithi:	lda HexScreenCode+1		; "1"
dbitlo:	and #$bf
	sta databits,y			; store bit screen code
	dey
	bpl dbitlp			; next bit
	rts
; ----------------------------------------------------------------------------
; print rom checksum
PrintByteHex:
	pha				; remember byte
	lsr				; isolate high nibble
	lsr
	lsr
	lsr
	ldy #$00
	jsr prnibbl			; print nibble
	pla
	and #$0f			; isolate low nibble
prnibbl:clc
	adc #$f6			; calculate screen code a-f
	bcs alpha			; greater 9 -> a-f
	adc #$39			; calculate screen code 0-9
alpha:	adc #$00
	and #$bf
	sta (pointer_screen2),y		; print to screen
	iny				; next position
	rts
; ----------------------------------------------------------------------------
; inc counter, clear screen
IncCounterClearScreen:
	ldx #$07
inc10:  inc ScreenRAM+1*80+16,x
	lda ScreenRAM+1*80+16,x
	and #$7f			; non inverse
	cmp #$3a			; >0
	bcc clrscr			; no overflow
	lda #$b0
	sta ScreenRAM+1*80+16,x		; inverse 0
	dex
	bpl inc10			; next higher number
	bmi IncCounterClearScreen	; max cycles -> endless loop
clrscr:	ldx #$00
	lda #" "			; clear screen from line 3
clrsclp:sta ScreenRAM+2*80,x
	sta ScreenRAM+$100,x
	sta ScreenRAM+$200,x
	sta ScreenRAM+$300,x
	sta ScreenRAM+$400,x
	sta ScreenRAM+$500,x
	sta ScreenRAM+$600,x
	sta ScreenRAM+$700,x
	inx
	bne clrsclp
	rts
; ----------------------------------------------------------------------------
; delay a times
Delay:  ldx #$ff
	ldy #$ff
delaylp:dex
	bne delaylp
	dey
	bne delaylp
	sec
	sbc #$01			; a times
	bne delaylp
	rts
; ----------------------------------------------------------------------------
; print dram bad address
PrintAddress:
	lda #45
	jsr AddChars			; add 45 chard
	ldx #>TextAddress
	ldy #<TextAddress
	lda #8
	jsr PrintTexta			; print "address"
	lda #55
	jsr AddChars			; add 5 chars
	lda pointer1+1
	jsr PrintByteHex		; print low byte
	lda #57
	jsr AddChars			; add 57 chars
	lda pointer1
	jsr PrintByteHex		; print high byte
	rts
; ************************************* ZONE TABLES ***********************************************
!zone tables
; messages
TitleHP256:	!scr " COMMODORE CBM 700 (256K) DIAGNOSTIC"

TitleHP128:	!scr " COMMODORE CBM 700 (128K) DIAGNOSTIC"

TitleLP256:	!scr " COMMODORE CBM 600 (256K) DIAGNOSTIC"

TitleLP128:	!scr " COMMODORE CBM 600 (128K) DIAGNOSTIC"

TextCycles:	!scr "  CYCLE "

Text000001:	!scr "  000001"

TextZeropage:	!scr " ZEROPAGE        "
		!scr " STACKPAGE       "	; not used
!ifdef STATICFULL{
TextStaticRam:	!scr " STATIC RAM 2KB  "	; enhanced full 2kB test
} else{
TextStaticRam:	!scr " STATIC RAM      "
}
TextVideoRam:	!scr " VIDEO  RAM      "
TextBasicRomL:	!scr " BASIC  ROM (L)  "
TextBasicRomH:	!scr " BASIC  ROM (H)  "
TextKernalRom:	!scr " KERNAL ROM      "
TextKeypoard:	!scr " KEYBOARD        "
TextIeeePort:	!scr " IEEE PORT       "
TextUserPort:	!scr " USER PORT       "
TextRS232:	!scr " RS-232          "
TextCassette:	!scr " CASSETTE        "
TextSoundchip:	!scr " SOUND CHIP      "
		!scr " VDC   CHIP      "
TextDram:	!scr " DRAM SEGMENT    "
TextTimers:	!scr " TIMERS          "
TextInterrupt:	!scr " INTERRUPT       "
TextAddress:	!scr " ADDRESS         "
TextDatabits:	!scr " DATABITS:       "

TextOK:	!scr " OK "

TextBad:!scr " BAD"

HexScreenCode:	!scr "0123456789ABCDEF"

	!byte $aa, $aa
; ----------------------------------------------------------------------------
; end of cart
*= $3fff
	!byte $ff
