;;; @file menu.s
;;; Menu program for the Vic UltiMem
;;; @author Marko Mäkelä (marko.makela@iki.fi)

;;; This file can be compiled with xa
;;; (Cross-Assembler 65xx V2.1.4h 12dec1998 (c) 1989-98 by A.Fachat)
;;; or xa (xa65) v2.3.5
;;; Written by Andre Fachat, Jolse Maginnis, David Weinehall and Cameron Kaiser

;;; Copyright © 2001-2016 Marko Mäkelä (marko.makela@iki.fi)
;;;
;;;	This program is free software; you can redistribute it and/or modify
;;;	it under the terms of the GNU General Public License as published by
;;;	the Free Software Foundation; either version 2 of the License, or
;;;	(at your option) any later version.
;;;
;;;	This program is distributed in the hope that it will be useful,
;;;	but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;	GNU General Public License for more details.
;;;
;;;	You should have received a copy of the GNU General Public License along
;;;	with this program; if not, write to the Free Software Foundation, Inc.,
;;;	51 Franklin Street, Suite 500, Boston, MA 02110-1335 USA.

#include "ultimem_regs.s"

NAMEBYTES = 16			; 16 bytes per file name
DIRENTSIZE	= 7+NAMEBYTES	; size of a directory entry in bytes
cBg		= 8	; background colour
cBgS		= $6e	; background for selected entry
cFg		= 1	; foreground colour
cFgD		= 7	; foreground colour for subdirectory entry

icasbuf	= $b2			; pointer to the cassette buffer
casbuf	= $33c			; start of the cassette buffer

mout	= $d1			; pointer for writing menu characters
cout	= $c3			; pointer for colourising menu entries
mbits	= mout			; bit store for menu characters
tmp	= $f7			; temporary location used by getbyt1
typefl	= $f9			; directory entry type
tmpos	= typefl		; temporary position, 24 bits
menupos	= $fc			; menu position, 24 bits
pagesiz	= $ff			; page size in bytes is MENULINES / 2

*	= $a000
	.word reset		; RESET vector
	.word reset		; NMI vector
	.byte $41,$30,$c3,$c2,$cd; cartridge signature a0CBM

;;; Constants for the menu interface, for both PAL and NTSC
FIRST_LINE	= 28	; first visible raster line
FIRST_COLUMN_P	= 5	; leftmost visible screen coordinate, PAL
LAST_COLUMN_P	= 65	; leftmost right border coordinate that is not visible
LINES_P		= 312	; number of scanlines
LINECYCLES_P	= 71	; number of bus cycles per scanline

FIRST_COLUMN_N	= 1	; leftmost visible screen coordinate, NTSC
LAST_COLUMN_N	= 55	; leftmost right border coordinate that is not visible
LINES_N		= 261	; number of scanlines
LINECYCLES_N	= 65	; number of bus cycles per scanline

VIA_RELOAD_TIME	= 2	; reloading a VIA timer takes 2 bus clock cycles
TIMER_VALUE_P	= LINES_P * LINECYCLES_P - VIA_RELOAD_TIME
TIMER_VALUE_N	= LINES_N * LINECYCLES_N - VIA_RELOAD_TIME

MENUWIDTH	= 16	; number of characters per menu entry
MENULINES_P	= 31	; number of menu entries on the screen, PAL
MENULINES_N	= 29	; number of menu entries on the screen, NTSC
RASTERLINE_P	= (FIRST_LINE + LINES_P) / 4 - 2 * MENULINES_P + 4 * (MENULINES_P / 2) - 6
SCREENTOP_P	= (FIRST_LINE + LINES_P) / 4 - 2 * MENULINES_P
SCREENLEFT_P	= (LAST_COLUMN_P + FIRST_COLUMN_P) / 2 - MENUWIDTH

SCREENTOP_N	= (FIRST_LINE + LINES_N) / 4 - 2 * MENULINES_N
SCREENLEFT_N	= (LAST_COLUMN_N + FIRST_COLUMN_N) / 2 - MENUWIDTH
RASTERLINE_N	= (FIRST_LINE + LINES_N) / 4 - 2 * MENULINES_N + 4 * (MENULINES_N / 2) - 6

MENUSTART	= $1e00	; start of menu in screen memory
COLORSTART	= $9600	; start of menu in color memory
MENUEND_P	= MENULINES_P * MENUWIDTH + MENUSTART
MENUEND_N	= MENULINES_N * MENUWIDTH + MENUSTART

bstart	= $2b	 	; start of BASIC program text
bend	= $2d		; end of basic program text
membot	= $282		; start page of BASIC RAM
memtop	= $284		; end page of BASIC RAM
screen	= $288		; start page of text matrix
repeat	= $28a		; key repeat ($80=repeat all keys)
mode	= $291		; upper/lower case change by shift+c= ($80=disabled)

warmstt	= $c7ae		; BASIC warm start
ireset	= $fffc		; RESET vector

;;; The entry point to the cartridge
reset
	;; clean up the VIA chips and the processor
	sei
	ldx #$ff
	txs			; initialize the stack pointer
	cld
	lda #$7f
	sta $913d		; disable VIA interrupts
	sta $913e		; acknowledge VIA interrupts
	lda #0
	sta $900e		; mute the audio
	sta $9002		; blank the screen (zero columns)
	.(
	ldx #13
loop	sta ultimem_blk,x	; initialize the UltiMem registers
	dex
	bne loop
	ldx #ultimem_cfg_led
	stx ultimem_cfg		; light up the LED
#if ultimem_ioram_0k
#error "ultimem_ioram_0k != 0"
#endif
	sta ultimem_ioram	; nothing at RAM123, I/O2, I/O3
	ldx #ultimem_blk_3_5_0k	; BLK3 ROM, BLK5 ROM; nothing at BLK1, BLK2
	stx ultimem_blk
	.)
	.(
	ldx #$a
loop	sta $9132,x		; initialize the VIA registers
	dex
	bpl loop
	.)			; X=$ff
	stx $9122		; data direction register B (keyboard column)

	;; check whether a key is pressed
#if ultimem_cfg_noled
#error "ultimem_cfg_noled != 0"
#endif
	tay			; Y=0 => UltiMem registers enabled, LED off
	.(
	lda #$fe
	ldx #$fd		; key '3' pressed?
	sta $9120
	cpx $9121
	bne no3k
	jmp boot3k		; 3 kilobytes
no3k	lda #$7f
	ldx #$f7		; key '8' pressed?
	sta $9120
	cpx $9121
	bne no8k
	jmp boot8k		; 8+ kilobytes (24 KiB)
no8k	lda #$fe		; key STOP pressed?
	stx $9120
	cmp $9121
	bne menu
	jmp boot0k		; unexpanded RAM
	;; no hot key pressed => go to the menu
menu	tya			; Y=0
clram	sta 0,y			; fill $000..$3ff with $00
	sta !$100-2,y		; preserve the return address
	sta $200,y
	sta $300,y
	iny
	bne clram
	.)

	;; initialize the screen output
	lda #cBg
	sta $900f		; black border and background
	sta $9000		; non-interlaced screen mode
	lda #$f2
	sta $9005		; screen memory at $1e00, lower case characters

	;; detect PAL/NTSC
	.(
waitnz	lda $9004		; wait for non-zero raster line
	beq waitnz
waitlast			; wait for last raster line
	lda $9004
	beq gotlast		; the raster counter wrapped around
	tay			; remember the raster line
	bne waitlast		; branch always

gotlast	cpy #(LINES_P - 1) / 2	; now y contains the maximum raster value
	beq tvpal
	cpy #(LINES_N - 1) / 2
	bne unknown
	jmp tvntsc
	;; unsupported video chip => display an error pattern
unknown	sta $900f
	sty $900f
	bne unknown
	.)

	;; video initialization
tvinit	sta $9003		; MENULINES lines of 8 pixels high characters
	stx $9001		; top screen line
	sty $9000		; left screen column
	lsr
	lsr
	sta pagesiz
	jsr $ff8a		; initialize the KERNAL jump vectors
	.(
	lda #<irq_p
	ldx #>irq_p
	ldy $9000
	cpy #SCREENLEFT_P
	beq setirq
	lda #<irq_n
	ldx #>irq_n
setirq	sta $314
	stx $315
	.)
	lda #>MENUSTART
	sta screen
	jsr $e536		; initialize the KERNAL screen and keyboard
	lda #$80
	sta repeat		; make all keys repeat
	sta mode		; disable upper/lower case changes (shift+c=)

	;; copy the auxiliary code
	lda #$ad		; lda
	sta INPOS-1
	lda #$60		; rts
	sta INPOS+2
	lda #<menuvec
	sta menupos
	lda #>menuvec
	sta menupos+1
	lda #(menuvec >> 16)
	sta menupos+2
	jsr display		; display the menu
	.(
	lda #(FIRST_LINE / 2) - 1
vblank	cmp $9004
	bcc vblank		; wait for vertical blank area
	.)
	lda #$90
	sta $9002		; 16 characters per line, screen address $1e00
	lda #$40
	sta $912b		; enable Timer A free run on VIA 1
	rts

	;; PAL mode
tvpal	lda #MENULINES_P * 2
	ldx #SCREENTOP_P
	ldy #SCREENLEFT_P
	jsr tvinit

	;; synchronise with the screen
	.(
	ldx #RASTERLINE_P
init	cpx $9004
	beq init
	.)
	.(
coarse	cpx $9004
	bne coarse
	.)
	.(
	ldy #9
	bit $24
fine	lda $9003		; loop to wait LINECYCLES_P=71 or 70 cycles
	tax
	tax
	ldx #10
delay	dex			; wait x*5-1 cycles
	bne delay
#if (*/256) - (delay/256)
#error "page boundary crossed, timing affected"
#endif
	cmp $9003
	beq * + 2
	dey
	bne fine
#if (*/256) - (fine/256)
#error "page boundary crossed, timing affected"
#endif
	.)

	lda #<TIMER_VALUE_P
	ldx #>TIMER_VALUE_P
	ldy #6
	nop
	nop
	nop
bnetvst	bne tvstart

	;; NTSC mode
tvntsc	lda #MENULINES_N * 2
	ldx #SCREENTOP_N
	ldy #SCREENLEFT_N
	jsr tvinit

	;; synchronise with the screen
	.(
	ldx #RASTERLINE_N
init	cpx $9004
	beq init
	.)
	.(
coarse	cpx $9004
	bne coarse
	.)
	.(
	ldy #9
	bit $24
fine	lda $9003		; loop to wait LINECYCLES_N=65 or 64 cycles
	tax
	tax
	tax
	tax
	ldx #8
delay	dex			; wait x*5-1 cycles
	bne delay
#if (*/256) - (delay/256)
#error "page boundary crossed, timing affected"
#endif
	cmp $9003
	beq * + 2
	dey
	bne fine
#if (*/256) - (fine/256)
#error "page boundary crossed, timing affected"
#endif
	.)

	lda #<TIMER_VALUE_N
	ldx #>TIMER_VALUE_N
	ldy #LINECYCLES_N / 5 - 1

tvstart
#if (bnetvst / 256) - (tvstart / 256)
#error "page boundary crossed, timing affected"
#endif
	dey
	bne * - 1
#if (*/256) - ((*-3) / 256)
#error "page boundary crossed, timing affected"
#endif
	sta $9126		; load the timer low byte latch
	stx $9125		; start Timer A

	lda #$c0
	cli
	sta $912e		; enable Timer A underflow interrupts
	bne getchar		; the first call to "display" is redundant

	;; main loop
update	jsr display
getchar	jsr $ffe4
	.(
	beq getchar
	ldx pagesiz
	ldy #0
	cmp #$13
	bne nohome
	;; home => move to the beginning of the list
home	jsr prev
	bcc home
	bcs update
nohome	cmp #$93
	bne noend
	;; clr => move to the end of the list
end	jsr next
	bcc end
	bcs update
noend	cmp #$11
	bne nodown
	;; down => move down by one entry
	jsr next
	bcc update
	bcs getchar
nodown	cmp #$1d
	bne nonext
	;; right => next page
nextp	jsr next
	bcs update
	dex
	bne nextp
	beq update		; branch always
nonext	cmp #$91
	bne noup
	;; up => move up by one entry
	jsr prev
bccupdate
	bcc update
	bcs getchar
noup	cmp #$9d
	bne noprev
	;; left => previous page
prevp	jsr prev
	bcs update
	dex
	bne prevp
bequpdate
	beq update
noprev	cmp #$0d
	beq go
	cmp #$8d
	bne nogo
	;; return => start the game
go	jmp launch

nogo	tax
	and #$3f		; mask the characters
	sta mbits
	cpx #$41
	bcc noch
	cpx #$5b
	bcc lch
	cpx #$c1
	bcc noch
	cpx #$db
	bcs noch
	;; upper-case character => move up until entry starts with char
	;; if at start, move to end
uch	jsr prev
	bcs end
	and #$3f
	eor mbits
	bne uch
	beq bequpdate
	;; lower-case character => move down until entry starts with char
	;; if at end, move to beginning
lch	jsr next
	bcs home
	and #$3f
	eor mbits
	bne lch
	beq bequpdate
noch	ldy #0			; UltiMem registers enabled, LED off
	cpx #$3
	beq boot0k		; stop => reset to unexpanded VIC-20
	cpx #$33
	beq boot3k		; 3 => reset to 3k expanded VIC-20
	cpx #$38
	beq boot8k		; 8 => reset to 8k+ expanded VIC-20
	ldy #ultimem_cfg_dis	; UltiMem registers disabled, LED off
	cpx #$83
	beq boot0k		; shift+stop => reset to unexpanded, disable
	cpx #$23		; shift+3 => reset to 3k expanded, disable
	beq boot3k
	cpx #$28
	beq boot8k		; shift+8 => reset to 8k expanded, disable
	cpx #$85		; f1
	bcc nofn
	cpx #$8c+1		; f8
	bcs nofn
	txa
	sbc #$85-1		; C=0
	sta mbits		; multiply A by 3
	asl
	adc mbits
	tax
	lda #<(fnvec / 8192)
	sta ultimem_blk3
	lda #>(fnvec / 8192)
	sta ultimem_blk3+1
	lda $6000+fnvec-allocb,x
	sta tmpos
	lda $6000+1+fnvec-allocb,x
	sta tmpos+1
	lda $6000+2+fnvec-allocb,x
	sta tmpos+2
	jsr lcheck
	bcs nofn		; uninitialized entry
	lda #$0d
	jmp launch
nofn	jmp getchar
	.)

	;; Boot into BASIC, with ultimem_cfg=Y
boot8k	ldx #1
	stx ultimem_blk2
	inx
	stx ultimem_blk3
	ldx #ultimem_blk_5_24k	; BLK5 ROM, BLK1..BLK3 RAM
	bne boot		; branch always
boot3k	ldx #ultimem_ioram_3k	; 3k RAM, nothing at I/O2, I/O3
	stx ultimem_ioram
boot0k	ldx #ultimem_blk_5_0k	; BLK5 ROM only
boot	stx ultimem_blk
	jsr cpreset
	stx $9002		; blank the screen (zero columns)
	sty reset_cfg
	lda ultimem_blk
	and #$3f
	sta reset_blk
	lda ireset+1
	pha
	lda ireset
	pha			; jump address
	lda #4
	pha			; Interrupt flag set, others clear
	jmp block_reset_

	;; display the menu, centered at the current entry (menupos)
display	ldy #0
	lda #>MENUSTART
	sty mout
	sta mout+1
	lda #>COLORSTART
	sty cout
	sta cout+1
	.(			; rewind by half a page
	ldx pagesiz
loop	jsr prev
	bcs done
	dex
	bne loop
done	.)
	txa			; X=number of blank lines to display at top
	pha
	.(
	beq done
loop	jsr blankln		; display blank lines at top if needed
	dex
	bne loop
done	.)
	pla
	tax
	jsr curr
	.(
loop	cpx pagesiz		; display menu entries at top
	clc
	beq done
	jsr println
	inx
	bne loop		; branch always
done	.)
	.(
	;; X=pagesiz
	jsr println		; display the current selection
	bcc loop
bloop	jsr blankln		; at the last menu entry,
	dex			; display X blank lines and return
	bne bloop
	rts
loop	jsr println		; display the entries following the selection
	bcs done
	dex
	bne loop
done	txa
done2	.)
	.(
	beq done
	pha
loop	jsr blankln		; display X blank lines at bottom
	dex
	bne loop
	pla
	tax
done	.)
	.(
loop	jsr prev		; rewind to the selected menu entry
	cpx pagesiz
	inx
	bcc loop
done	.)
	rts

	;; display a blank line, leave Y=0 and C=0, X unaffected, A trashed
blankln	ldy #15
	.(
loop	lda #$20		; space
	sta (mout),y
	lda #cFg
	sta (cout),y
	dey
	bpl loop
	.)
	iny
	clc			; advance to next line
	lda mout
	adc #MENUWIDTH
	sta mout
	sta cout
	.(
	bcc done
	inc mout+1
	inc cout+1
	clc
done	rts
	.)

;;; convert A from PETSCII to screen code using the following bijective mapping
;;; [map spare PETSCII codes to otherwise unused screen codes]
;;; $00-$1F	+128	$80	$80-$9F	ctrl
;;; $20-$3F		$00	$20-$3F	numbers and punctuation
;;; $40-$5F	-64	$C0	$00-$1F	characters
;;; $60-$7F	+64	$40	$A0-$BF	capital characters (unused)
;;; $80-$9F	+64	$40	$C0-$DF	shifted ctrl
;;; $A0-$BF	-64	$C0	$60-$7F	graphics
;;; $C0-$DF	-128	$80	$40-$5F	capital characters
;;; $E0-$FF		$00	$E0-$FF	graphics (unused)
pet2scr	cmp #$20
	.(
	bcs noctrl
invert	eor #$80		; $00..$1f -> $80..$9f (ctrl characters)
	rts
noctrl	cmp #$40
	bcc done		; $20..$3f -> $20..$3f (punctuation)
	cmp #$60
	bcc sub40		; $40..$5f -> $00..$1f (lower case)
	cmp #$a0
	bcs noalpha
	adc #$40		; $60..$9f -> $a0..$df
	rts
noalpha	cmp #$e0
	bcs done		; $e0..$ff -> $e0..$ff (graphics, unused)
	cmp #$c0
	bcs invert		; $c0..$df -> $40..$5f (upper case)
sub40	sbc #$40-1
done	rts
	.)

	;; display a menu entry, preserve X, assume Y=0, trash A, C=1 on EOF
println	ldy #0
	jsr pet2scr
	sta (mout),y		; the first character is in A
	.(
	lda typefl
	asl
	bcc notdir
	bpl notdir
	lda #cFgD		; color of subdirectories
	.byte $2c
notdir	lda #cFg		; color of other entries
	sta (cout),y		; set the color
	sta tmpos
loop	iny
	sec
	jsr getbyt
	jsr pet2scr
	sta (mout),y
	lda tmpos
	sta (cout),y
	cpy #MENUWIDTH-1
	bcc loop
	.)
	lda mout
	adc #NAMEBYTES-1		; C=1
	sta mout
	sta cout
	.(
	bcc done
	inc mout+1
	inc cout+1
done	ldy #0
	.)
	;; go to next menu entry, set C=1 if there is none
	;; preserve X and Y, A=first byte of encoded file name
next	jsr getbyt1
	jsr getbyt
	jsr getbyt
	jsr getbyt
	jmp prev2

	;; go to previous menu entry, set C=1 if there is none
	;; preserve X and Y, A=first byte of encoded file name
prev	jsr getbyt1
prev2	sta tmpos
	jsr getbyt
	sta tmpos+1
	jsr getbyt
	sta tmpos+2
lcheck	.(
	eor #$ff
	bne nonempty
	lda tmpos+1
	eor #$ff
	bne nonempty
	lda tmpos
	eor #$ff
	bne nonempty
	sec
	rts
nonempty
	.)
	lda tmpos
	sta menupos
	lda tmpos+1
	sta menupos+1
	lda tmpos+2
	sta menupos+2
	;; re-read the first character of current menu entry
curr	jsr getbyt1		; skip the prev and next links
	jsr getbyt
	jsr getbyt
	jsr getbyt
	jsr getbyt
	jsr getbyt
	jsr getbyt
	sta typefl		; store the flags
	jsr getbyt		; read the first byte of the menu entry
	clc
	rts			; return the first byte of the menu entry

	;; raster interrupt handler for 6560-101 (NTSC-M)
irq_n	lda $9124	; get the Timer A value
	sec		; (42 + 32 to 49 + 32 cycles delay at this stage)
	sbc #<TIMER_VALUE_N - 49 - 32 + VIA_RELOAD_TIME
	ldx #LINECYCLES_N*9/5-1	; delay to wait between changing colours
bneirqt	bne irqt		; branch always
	;; raster interrupt handler for 6561 (PAL-[BGI])
irq_p	sec		; (42 + 32 to 49 + 32 cycles delay at this stage)
	lda $9124	; get the Timer A value
	sbc #<TIMER_VALUE_P - 49 - 32 + VIA_RELOAD_TIME
	ldx #LINECYCLES_P*9/5-1	; delay to wait between changing colours
	;; now it has taken a fixed amount of cycles since the IRQ event
irqt	cmp #8		; are we more than 7 cycles ahead of time?
#if (bneirqt / 256) - (irqt / 256)
#error "page boundary crossed, timing affected"
#endif
	.(
saving8	bcc save8
	pha		; yes, spend 8 extra cycles
	pla
	and #7		; and reset the high bit
save8	cmp #4
#if (saving8 / 256) - (save8 / 256)
#error "error: page boundary crossed, timing affected"
#endif
saving4	bcc save4
	bit $24		; waste 4 cycles
	and #3
save4	cmp #2		; spend the rest of the cycles
#if (saving4 / 256) - (save4 / 256)
#error "page boundary crossed, timing affected"
#endif
	.)
#if (* / 256) - ((*+2) / 256)
#error "page boundary crossed, timing affected"
#endif
	bcs *+2
#if (* / 256) - ((*+2) / 256)
#error "page boundary crossed, timing affected"
#endif
	bcs *+2
	lsr
#if (* / 256) - ((*+2) / 256)
#error "page boundary crossed, timing affected"
#endif
	bcs *+2
	lda #cBgS
	sta $900f
	lda #cBg
	dex
#if (* / 256) - ((*-1) / 256)
#error "page boundary crossed, timing affected"
#endif
	bne *-1
	sta $900f
	jsr $ff9f	       ; read the keyboard
	pla
	tay
	pla
	tax
	pla
	rti

	;; convert the address and read the first character to A
getbyt1	lda menupos
	sta INPOS
	lda menupos+1
	sta tmp
	and #$1f
	ora #$60
	sta INPOS+1
	lda menupos+2
	lsr
	ror tmp
	lsr
	ror tmp
	lsr
	ror tmp
	lsr
	ror tmp
	lsr
	sta ultimem_blk3+1
	lda tmp
	ror
	sta ultimem_blk3
	;; read a byte from the ROM via BLK3
getbyt
	jsr INPOS-1		; read the byte
	.(
	inc INPOS
	bne gotbyt
	inc INPOS+1
	bpl gotbyt
	pha
	lda #>$6000		; wrap around to the start of BLK3
	sta INPOS+1
	pla
	inc ultimem_blk3
	bne gotbyt
	inc ultimem_blk3+1
gotbyt
	.)
	rts

	;; select a subdirectory
subdir	lda #DIRENTSIZE
	adc menupos		; C=0
	sta menupos
	lda menupos+1
	adc #0
	sta menupos+1
	lda menupos+2
	adc #0
	sta menupos+2
	jsr prev
	bcc nolaunch
	lda menupos		; invalid subdirectory pointer: restore it
	sbc #DIRENTSIZE		; C=1
	sta menupos
	lda menupos+1
	sbc #0
	sta menupos+1
	lda menupos+2
	sbc #0
	sta menupos+2
nolaunch
	pla			; discard the keypress ($0d or $8d)
	jmp update

	;; Launch the current program (at menupos)
launch	pha			; remember the keypress ($0d or $8d)
	jsr curr
	lda typefl
	eor #$ff
	beq subdir
	.(
	ldx #NAMEBYTES-1
loop	jsr getbyt		; skip the rest of the file name
	dex
	bne loop
	.)
	lda #$30
	.(
	bit typefl
	beq nolaunch		; memory expansion bits 00 are reserved
	bvs basic
	bpl nolaunch		; type flags 00 are not allowed
	jmp startcart

basic	bmi nolaunch		; type flags 11 are not allowed
	lda typefl
	eor #$ff
	and #$0f
	bne nolaunch		; all reserved bits must be 0
	.)
	;; Set the start and end address
	jsr getbyt
	tay			; LSB of the start address
	jsr getbyt
	pha			; MSB of the start address
	jsr getbyt
	sta bend
	jsr getbyt
	sta bend+1

	sei
	lda #<casbuf		; initialize the tape buffer location
	sta icasbuf		; like the KERNAL RAM test at $fd8d does
	lda #>casbuf
	sta icasbuf+1
	.(
	ldx #copy_end-copy	; copy the copying routine to RAM
loop	lda copy_-1,x
	sta !copy-1,x		; avoid zeropage addressing
	dex
	bne loop
	.)
	stx $9002		; blank the screen (zero columns)
	lda #$7f
	sta $913d		; disable VIA interrupts
	sta $913e		; acknowledge VIA interrupts
	pla
	sta OUT_HI		; MSB of the output address

	;; switch the addresses from BLK3 to BLK5
	lda #>$c000
	eor INPOS+1
	sta INPOS+1
	lda ultimem_blk3
	sta copy_blk_lo
	lda ultimem_blk3+1
	sta copy_blk_hi

	.(
	ldx #ultimem_blk_5_0k	; flash at BLK5; other BLK disabled (jsr copy)
	lda typefl
	and #$30		; memory expansion bits
	cmp #$20		; 8k+?
	bcc use3k
	bne copyit		; unexpanded

	;; 8k+ (24k) RAM
	inc ultimem_blk2
	lda #2
	sta ultimem_blk3
	ldx #ultimem_blk_5_24k	; BLK5 ROM, BLK1..BLK3 RAM
	bne copyit		; branch always
use3k	lda #ultimem_ioram_3k	; 3k RAM, nothing at I/O2, I/O3
	sta ultimem_ioram
copyit	txa
	pha
	jsr copy_start		; copy from flash to RAM
	.)
	jsr cpreset		; copy the trampoline code for starting

	pla
	and #$3f		; nothing mapped at BLK5
	tay
	sty reset_blk

	pla			; the keypress ($0d or $8d)
	and #$80
	sta reset_cfg		; disable the UltiMem registers on Shift+RETURN

	tya
	bne lexp8k		; some BLK is mapped
	;; unexpanded or 3k
	ldx #>$0400		; BASIC at $0400 (3k)
#if ultimem_ioram_0k
#error "ultimem_ioram_0k != 0"
#endif
	lda ultimem_ioram
	beq lexp0k
	.byte $2c
lexp0k	ldx #>$1000		; BASIC at $1000 (unexpanded)
	stx $97			; start of RAM
	lda #$20
	sta $c2			; end of RAM at $2000
	lda #>$1e00		; A => screen at $1e00
	tay			; Y => BASIC ends at $1e00
	bne lptrset		; branch always
lexp8k	lda #>$1000		; A => screen at $1000
	ldx #>$1200		; X => BASIC at $1200
	ldy #>$8000		; Y => BASIC ends at $8000
	sty $c2			; end of RAM at $8000
lptrset	sta screen
	stx membot
	sty memtop

	jsr $ff8a		; initialize the KERNAL jump vectors
	jsr $fdf9		; initialize the I/O chips
	jsr $e518		; initialize the screen
	jsr $e45b		; initialize jump vectors for BASIC
	jsr $e3a4		; initialize zero page for BASIC
	lda bstart
	ldy bstart+1
	jsr $c408		; check memory overlap
	jsr $c659		; CLR

	lda #>warmstt
	pha
	lda #<warmstt
	pha			; start address
	lda #0			; all flags clear
invoke	pha			; status register
	jmp block_reset_

;;; Code for copying an image from flash (BLK5) to RAM
copy_
INPOS = $101
copynext= INPOS-1
;	lda $ffff		; PARAMETER
*	= INPOS+2
OUT_HI = *+2
copy	sta $ff00,y		; PARAMETER
	.(
	inc INPOS
	bne inc_in
	inc INPOS+1
	bit INPOS+1
	bvc inc_in		; INPOS < $c000
	lda #>$a000
	sta INPOS+1
	inc ultimem_blk5
	bne inc_in
	inc ultimem_blk5+1
inc_in	iny
	bne inc_out
	inc OUT_HI
inc_out	cpy bend
	.)
	bne copynext
	lda OUT_HI
	eor bend+1
	bne copynext
	sta ultimem_blk5	; A=0
	sta ultimem_blk5+1	; A=0
	stx ultimem_blk		; ROM at BLK5
	rts
copy_start
	sta ultimem_blk
copy_blk_lo = *+1
	lda #$ff
	sta ultimem_blk5
copy_blk_hi = *+1
	lda #$ff
	sta ultimem_blk5+1
	jmp copynext
copy_end
*	= copy_end - copy + copy_

	;; cartridge image
startcart
	.(
	ldx #launchcart_end - launchcart
loop	lda launchcart_-1,x
	sta launchcart-1,x
	dex
	bne loop
	.)
	txa			; X=0
	ldx #15
	.(
loop	sta ultimem_l,x
	dex
	bne loop
	.)
	lda #ultimem_cfg_dis|ultimem_cfg_reset
	sta ultimem_l-ultimem+ultimem_cfg
	lda typefl
	and #$30		; memory expansion bits
	cmp #$20		; 8k+?
	bcc cart3k
	bne cartblk		; unexpanded

	;; 8k+ (24k) RAM
	lda #ultimem_blk_24k	; nothing at BLK5; RAM at BLK1..BLK3
	sta ultimem_l-ultimem+ultimem_blk
	inc ultimem_l-ultimem+ultimem_blk2
	lda #2
	sta ultimem_l-ultimem+ultimem_blk3
	bne cartblk		; branch always
cart3k
	lda #ultimem_ioram_3k	; 3k RAM, nothing at I/O2, I/O3
	sta ultimem_l-ultimem+ultimem_ioram
cartblk	lda typefl
	ora #$f0
	tax			; number of blocks (negative)
	.(
loop	jsr getbyt
	pha
	jsr getbyt
	pha
	and #$e0
	bpl ok
	pla
	pla
	jmp nolaunch
ok	lsr
	lsr
	lsr
	lsr
	tay
	pla
	sta ultimem_blk1+1-ultimem+ultimem_l,y
	pla
	sta ultimem_blk1-ultimem+ultimem_l,y
	;; store the memory configuration
	lda ultimem_l-ultimem+ultimem_blk
	and blkmask,y
	ora blkmask+1,y
	sta ultimem_l-ultimem+ultimem_blk
	inx
	bne loop
	.)
	ldx #15
	sei
	jmp launchcart

;;; Code for launching a cartridge image. X=15 upon entering.
launchcart_
ultimem_l = INPOS+3
*	= ultimem_l + 16
launchcart
	lda ultimem_l,x
	sta ultimem,x
	dex
	bpl launchcart
	jmp ($fffc)
launchcart_end
*	= launchcart_end - launchcart + launchcart_

	;; table=>bitmasks for the configuration register,
	;; and bit patterns for selecting ROM
	;; 0=BLK1,1=BLK2,2=BLK3,3=BLK5
blkmask	.byte $fc,$01,$f3,$04,$cf,$10,$3f,$40

	;; clear the low RAM, copy the RESET code
	;; restore normal key repeat and set the cassette buffer pointer
cpreset	sei
	tsx
	lda #0
	sta repeat		; restore normal key repeat
	.(
loop	dex
	sta $100,x		; clear the stack
	bne loop
	.)

	ldx #block_reset_end-block_reset_
	.(
loop	lda block_reset-1,x
	sta !block_reset_-1,x	; prohibit zero page addressing
	dex
	bne loop
	.)
	rts

;;; Trampoline code for starting the program
block_reset
*	= $100
block_reset_
reset_blk = *+1
	lda #0
	sta ultimem_blk
reset_cfg = *+1
	lda #0
	sta ultimem_cfg		; change the memory configuration
	rti
block_reset_end
*	= block_reset_end - block_reset_ + block_reset

;;; An allocation bitmap (256 bytes; 1 bit for each 8-kilobyte page)
;;; is located at address $10000 (65536).
allocb	= $10000

;;; Singly linked list of free memory, terminated by $ffffff.
;;; The memory from the $ffffff entry to the end of the 8-kilobyte block
;;; is available.
freevec	= $10100
fnvec	= freevec + 3		; Function key map
menuvec	= fnvec + 3 * 8		; Start address of the root menu

;;; Singly linked list of free memory, terminated by $ffffff
; (meaning that all memory following the $ffffff is free)
;freemem
;	.byte $ff, $ff, $ff	;all memory free
;fnmap	.byte $ff, $ff, $ff	;shortcut for f1 (0xffffff=none)
;	.byte $ff, $ff, $ff	;shortcut for f3 (0xffffff=none)
;	.byte $ff, $ff, $ff	;shortcut for f5 (0xffffff=none)
;	.byte $ff, $ff, $ff	;shortcut for f7 (0xffffff=none)
;	.byte $ff, $ff, $ff	;shortcut for f2 (0xffffff=none)
;	.byte $ff, $ff, $ff	;shortcut for f4 (0xffffff=none)
;	.byte $ff, $ff, $ff	;shortcut for f6 (0xffffff=none)
;	.byte $ff, $ff, $ff	;shortcut for f8 (0xffffff=none)
;rootmenu
	;; 3 bytes => link to previous entry (0xffffff=none)
;	.byte $ff, $ff, $ff	; no previous entry
	;; 3 bytes => link to next entry (0xffffff=none)
;	.byte $ff, $ff, $ff	; no next entry
	;; 1 byte => flags
	;; b7..b6 => type of directory entry
	;;  00=reserved
	;;  01=BASIC program
	;;  10=cartridge
	;;  11=subdirectory (pointer to first entry follows)
	;; b5..b4 => type of memory expansion (ignored for subdirectories)
	;;  00=reserved
	;;  01=3k
	;;  10=24k
	;;  11=no expansion (or the entry is a subdirectory)
	;;  There will be RAM at BLK5 in any case.
;	.byte $ff		; subdirectory
	;; this is a subdirectory => include link to the subdirectory
;	.byte $ff, $ff, $ff	; subdirectory pointer (none)
