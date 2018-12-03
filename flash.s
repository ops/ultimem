;;; @file flash.s
;;; Flash programming software for the Vic UltiMem
;;; @author Marko Mäkelä (marko.makela@iki.fi)

;;; This file can be compiled with xa
;;; (Cross-Assembler 65xx V2.1.4h 12dec1998 (c) 1989-98 by A.Fachat)
;;; or xa (xa65) v2.3.5
;;; Written by Andre Fachat, Jolse Maginnis, David Weinehall and Cameron Kaiser

;;; Copyright © 2003,2010-2012,2015-2016 Marko Mäkelä (marko.makela@iki.fi)
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

#if FILEIO
st	= $90			; I/O status word
curlf	= $b8			; current logical file
curfa	= $ba			; current first address (device number)
cursa	= $b9			; current secondary address

flash	= $6000			; base address of the flash ROM (BLK3)
ultimem_bank = ultimem_blk3
ultimem_blk_def = $10		; flash at BLK3; no expansion RAM
ultimem_blk_mask = $cf
#else
flash	= $a000			; base address of the flash ROM (BLK5)
ultimem_bank = ultimem_blk5
ultimem_blk_def = $7f		; flash at BLK5, 24k RAM at BLK[123]
ultimem_blk_mask = 0
#endif

f000	= flash			; flash address 0
toggle	= $40			; the flash toggle bit (DQ6)

crsrchr	= $d1			; cursor position in screen memory
crsr_x	= $d3			; current cursor column
crsrcol	= $f3			; cursor position in color memory
txtcolor= 646			; current text color
pout	= $fb			; pointer to flash address (2 bytes)
#if FILEIO
size	= $fd			; size of block being programmed (16 bits)
#else
size	= $a8			; size of block being programmed (16 bits)
pin	= $fd			; pointer to source address (2 bytes)
#endif
tmp	= $ff			; auxiliary memory
tmp2	= $100			; auxiliary memory

;;; I/O routines
strout	= $cb1e			; output a NUL-terminated string
#if FILEIO
setmsg	= $ff90			; control KERNAL messages
setlfs	= $ffba			; set logical, first and second address
b_setnam= $e254			; parse and set file name in BASIC
b_comma	= $cefd			; test for comma
b_getx	= $d79e			; evaluate text to 1 byte in X
;setnam	= $ffbd			; set file name
open	= $ffc0			; open a file
close	= $ffc3			; close a specified logical file
chkin	= $ffc6			; open channel for input
clrchn	= $ffcc			; clear input and output redirection
chrin	= $ffcf			; input character from channel
#endif
chrout	= $ffd2			; output a character

cTEXT	= 1			; white text color
cPROG	= 5			; green progress indicator color
cOK	= 0			; black screen signifies OK color
cBUSY	= 1			; white border signifies a wait
cERR	= 2			; red border signifies error
color	= $900f			; color register

;;; set the screen color to c
#define setcolor(c) lda #8+c:sta color
;;; print a NUL-terminated message
#define printmsg(x) lda#<x:ldy#>x:jsr strout
#define printmsg_rts(x) lda#<x:ldy#>x:jmp strout

#if FILEIO
prgstart = $1001
#else
prgstart = $1201
#endif
	.word prgstart
	*=prgstart
prg	.word nextln
	.word 2016
	.byte $9e		; SYS
	.byte $30 + (start / 1000)
	.byte $30 + ((start - (start / 1000 * 1000)) / 100)
	.byte $30 + ((start - (start / 100 * 100)) / 10)
	.byte $30 + (start - (start / 10 * 10))
#if FILEIO
	.byte $22		; image file name in quotes
fn
	.byte "ULTIMEM.BIN"
	.byte $22, ",00"
	.dsb fn-*+20, $3a
#endif
	.byte 0
nextln	.word 0
#if FILEIO
start	jsr b_setnam		; set the file name
	jsr b_comma
	jsr b_getx
	txa
	.(
	bne havedev
	lda curfa
	bne havedev
	printmsg_rts(specify_device)
havedev	sta curfa
	.)
#else
	.word ops		; pointer to the operation script
start
#endif
	sei
	lda $9f55		; Re-enable the UltiMem registers
	lda $9faa		; if they were disabled previously.
	lda $9f01
	.(
	ldx #unit_id_end-unit_id-1
	lda ultimem_id
id	cmp unit_id,x
	beq id_ok
	dex
	bpl id
	cli
	printmsg_rts(ultimem_not_detected)
id_ok	lda autosel_lo,x
	sta iautosel
	lda autosel_hi,x
	sta iautosel+1
	.)
	txa
	pha
	lda ultimem_blk
	sta ultimem_blk_save
	lda ultimem_bank
	sta ultimem_blk_lo_save
	lda ultimem_bank + 1
	sta ultimem_blk_hi_save
	setcolor(cOK)
	lda #cTEXT
	sta txtcolor
#if ultimem_blk_mask
	lda ultimem_blk
	and #ultimem_blk_mask
	ora #ultimem_blk_def
#else
	lda #ultimem_blk_def
#endif
	sta ultimem_blk		; flash ROM enable
	lda #0
	sta ultimem_bank	; bank select low
	sta ultimem_bank + 1	; bank select high
#if ultimem_cfg_noled
	lda #ultimem_cfg_noled
#endif
	sta ultimem_cfg
	ldy #$f0
	sty flash		; poke x, $f0 (read array data)
	ldx #$90		; $90 (autoselect)
	jsr command
	pla
	tax
	lda flash		; read flash address 0 (manufacturer id)
	cmp mfr_id,x
	beq mfr_ok

	sty flash		; poke x, $f0 (read array data)
	pha
	printmsg(mfr_mismatch)
	pla
	jsr hex2
	jmp error_println_exit

mfr_ok	lda dev_id_offs,x
	tay
	lda flash,y		; read device ID
	ldy #$f0
	sty flash		; poke x, $f0 (read array data)
	cmp dev_id,x
	beq dev_ok

	pha
	printmsg(dev_mismatch)
	pla
	jsr hex2
	jmp error_println_exit

dev_ok
#if FILEIO
	ldx #1
	stx curlf
	dex
	stx cursa
	lda #$c0
	jsr setmsg
	jsr open
	bcc open_ok
	jmp file_read_error
open_ok	ldy st
	bne file_read_error
	ldx #1
	jsr chkin
	bcc next_op
	jmp error_println_exit
next_op	jsr read		; read the next op code
	jsr op_do		; process it
	bit st
	bvc next_op
	setcolor(cOK)
	printmsg(operation_complete)
	jmp exit

read	bit st
	bvs read_fail		; end of file
	jsr chrin
	ldy st
	bne read_fail
read_ok	rts
read_fail
	pha
	tya
	cmp #$40		; got last byte?
	pla
	beq read_ok
	pla
	pla
file_read_error
	printmsg(file_read)
#else
	lda #<ops
	sta pin
	lda #>ops
	sta pin+1
	ldx #0
	lda (pin,x)		; read the first opcode without preincrement
	jsr op_do
oploop	ldx #0
	jsr read
	jsr op_do
	jmp oploop
	;; read the next byte at the input position (.X must be 0)
read	inc pin
	bne readpin
	inc pin+1
	bpl readpin
	printmsg(input_overflow)
	.byte 2			; hang to prevent further corruption
readpin	lda (pin,x)
	rts
#endif
error_println_exit
	lda #$0d		; output a carriage return (line break)
	jsr chrout
	setcolor(cERR)		; flag error status
exit				; exit to the operating system
#if FILEIO
	lda #1
	jsr close
	jsr clrchn
#endif
	lda #ultimem_cfg_noled
	sta ultimem_cfg
	lda #0
ultimem_blk_save = *-1
	sta ultimem_blk
	lda #0
ultimem_blk_lo_save = *-1
	sta ultimem_bank
	lda #0
ultimem_blk_hi_save = *-1
	sta ultimem_bank + 1
	cli
	rts

	;; execute the operation in .A
op_do	pha
#if FILEIO
#else
	lda pin+1
	jsr hex2
	lda pin
#endif
	jsr hex2
	lda #$20
	jsr chrout
	pla
	cmp #(ops-optab)/2
	bcs op_unknown
	asl
	tax
	lda optab+1,x
	pha
	lda optab,x
	pha
#if ultimem_cfg_led - ultimem_cfg_noled - 1
	lda #ultimem_cfg_led
	sta ultimem_cfg
#else
	inc ultimem_cfg
#endif
	rts
op_unknown
	printmsg(unknown_op)
	pla			; discard the return address
	pla			; (return to caller's caller)
	jmp error_println_exit

	;; program data
program
#if FILEIO
#else
	ldx #0
#endif
	jsr read		; parameter => size (16 bits)
	sta size
	jsr read
	sta size+1
	jsr read		; parameter => address (24 bits)
	sta pout
	jsr read
	sta pout+1
	sta tmp
	jsr read
	sta tmp2
	lda #0
	asl tmp
	rol tmp2
	rol
	asl tmp
	rol tmp2
	rol
	asl tmp
	rol tmp2
	rol
	sta ultimem_bank + 1
	lda tmp2
	sta ultimem_bank
	lda pout+1
	and #$1f
	ora #>flash
	sta pout+1
	setcolor(cBUSY)
	printmsg(programming)
	lda size+1
	jsr hex2
	lda size
	jsr hex2
	printmsg(programming_size)
	jsr printaddr
	printmsg(programming_size_end)
	inc size
	inc size+1
	ldx #0
prog	clc
	dec size		; all bytes programmed?
	bne progn
	dec size+1
	beq op_complete
progn	jsr read
	tay
#if FILEIO
	ldx #0
#endif
	eor (pout,x)
	beq progok		; already programmed (skip)
	ldx #$a0
	jsr command		; command (write byte)
	tya
	ldx #0
	sta (pout,x)		; We cannot use post-indexed mode here,
				; because it would read from the address first.
ppoll	lda f000		; poll the operation status
	eor f000
	and #toggle		; bits toggling?
	bne ppoll		; yes, keep polling
	lda #$f0		; switch to reading array data
	sta flash		; poke x, $f0 (read array data)
	tya
	cmp (pout,x)		; verify the data
	bne verror
progok	inc pout		; increment the address, wrap if necessary
	bne prog
	inc pout+1
#if flash - $6000
#if flash - $a000
	lda pout+1
	eor #>(flash + $2000)
	bne prog
#else
	bit pout+1
	bvc prog		; wrap around at flash+$2000 = $c000
#endif
#else
	bpl prog		; wrap around at flash+$2000 = $8000
#endif
	lda #>flash		; wrap to the start of the flash address
	sta pout+1
	inc ultimem_bank
	bne prog
	inc ultimem_bank + 1
	bne prog		; branch always
op_all_complete
	pla			; discard the return address
	pla			; (return to caller's caller)
#if FILEIO
#if ultimem_cfg_led - ultimem_cfg_noled - 1
	lda #ultimem_cfg_noled
	sta ultimem_cfg
#else
	dec ultimem_cfg
#endif
	setcolor(cOK)
	printmsg(operation_complete)
	jmp exit
#endif
op_complete
#if ultimem_cfg_led - ultimem_cfg_noled - 1
	lda #ultimem_cfg_noled
	sta ultimem_cfg
#else
	dec ultimem_cfg
#endif
	setcolor(cOK)
	printmsg_rts(operation_complete)

verror	pla			; discard the return address
	pla			; (return to caller's caller)
	printmsg(verify_failure)
	jsr printaddr
#if FILEIO
#else
	printmsg(verify_failure_wrote)
	ldx #0
	lda (pin,x)
	jsr hex2
#endif
	printmsg(verify_failure_read)
	ldx #0
	lda (pout,x)
	jsr hex2
	jmp error_println_exit

erase	printmsg(erase_in_progress)
	ldx #$80		; command (erase)
	jsr command
	ldx #$10		; parameter (erase chip)
	jsr command
erasepoll
	setcolor(cBUSY)
	lda #cPROG
	ldy crsr_x
	sta (crsrcol),y		; set progress indicator color
epollx	ldx #progress_end - progress - 1
epoll	lda f000		; poll the device until operation complete
	eor f000
	and #toggle		; bits toggling?
	beq erased		; no, completed
	lda progress,x
	sta (crsrchr),y
	dex
	bpl epoll
	bmi epollx
	ldy #$f0
	sty flash		; poke x, $f0 (read array data)
erased	ldx flash
	cpx #$ff
	beq op_complete
	pla			; discard the return address
	pla			; (return to caller's caller)
	txa
	pha
	printmsg(erase_failure)
	pla
	jsr hex2
	jmp error_println_exit

	;; erase a block
erase_blk
	printmsg(erase_sec_in_progress)
	jsr read
	pha
	jsr hex2
	lda #0
	sta tmp
	pla
	asl
	rol tmp
	asl
	rol tmp
	asl
	rol tmp
	sta ultimem_bank
	lda tmp
	sta ultimem_bank + 1
	printmsg(erase_sec_in_progress_at)
	lda #0
	sta pout
	sta pout+1
	jsr printaddr
	printmsg(erase_in_progress2)
	ldx #$80		; command (erase)
	jsr command
	ldx #$30		; parameter (sector, specified with A23..A16)
	jsr command
	jmp erasepoll

	;; print the current flash ROM address
printaddr
	lda pout+1
	asl
	asl
	asl
	sta tmp2
	lda ultimem_bank
	sta tmp
	lda ultimem_bank + 1	; read the most significant bits of the address
	lsr
	ror tmp
	ror tmp2
	lsr
	ror tmp
	ror tmp2
	lsr
	ror tmp
	ror tmp2
	lda tmp
	jsr hex2
	lda tmp2
	jsr hex2
	lda pout
	;; print two hexadecimal digits in .A
hex2
	tax
	lsr
	lsr
	lsr
	lsr
	jsr hex1
	txa
	;; print one hexadecimal digit in .A
hex1
	and #$f
	ora #$30
	cmp #$3a
	bcc doprint
	adc #$41-$3a-1
doprint
	jmp chrout

	;; issue a command in .X, trash .A
command	lda #$aa
	jmp autosel		; self-modifying code!
iautosel = *-2			; jump vector to the flash autoselect sequence
autosel	sta flash+$555		; magic sequence for the Am29F040B
	lda #$55
	sta flash+$2aa
	stx flash+$555
	rts
autose1	sta flash+$aaa		; magic sequence for the S29GL064N
	lda #$55
	sta flash+$555
	stx flash+$aaa
	rts

ultimem_not_detected
	.byte "ULTIMEM NOT DETECTED", 0

	;; UltiMem unit type identifier
unit_id	.byte ultimem_id_512k, ultimem_id_8m
unit_id_end
	;; flash autoselect sequence for each unit_id
autosel_lo	.byte <autosel, <autose1
autosel_hi	.byte >autosel, >autose1
	;; manufacturer ID for each unit_id
mfr_id	.byte 1, 1
	;; address of the flash device ID byte
dev_id_offs
	.byte 1, 2
	;; device ID
dev_id	.byte $a4, $7e		; Am29F040B, S29GL064N

	;; erase progress indicator
progress
	.byte 109,112,110,125
progress_end

CR	= 13			; Carriage return
WHITE	= 5			; Switch to white text (neutral)
RED	= 28			; Switch to red text (error)
GREEN	= 30			; Switch to green text (successful)

mfr_mismatch
	.byte "UNKNOWN MANUFACTURER,", CR, "GOT ID ", 0
dev_mismatch
	.byte "UNKNOWN DEVICE,", CR, "GOT ID ", 0
unknown_op
	.byte RED, "UNKNOWN OP", WHITE, 0
#if FILEIO
specify_device
	.byte "NEED INPUT FILE&DEVICE",0
file_read
	.byte RED, "FILE READ FAILURE", WHITE, 0
#else
input_overflow
	.byte "INPUT BUFFER OVERFLOW", CR, 0
#endif
erase_sec_in_progress
	.byte "ERASE SEC ", 0
erase_sec_in_progress_at
	.byte CR, "AT ", 0
erase_in_progress
	.byte "ERASE"
erase_in_progress2
	.byte ": ", 0
operation_complete
	.byte GREEN, "OK", WHITE, CR, 0
erase_failure
	.byte RED, "FAILED", WHITE, CR, "FIRST CELL IS ", 0
programming
	.byte "WRITE ", 0
programming_size
	.byte " BYTES", CR, "TO ", 0
programming_size_end
	.byte ": ", 0
verify_failure
	.byte RED, "FAILED", WHITE, CR, "AT ", 0
#if FILEIO
#else
verify_failure_wrote
	.byte CR, "WROTE: ", 0
#endif
verify_failure_read
	.byte " READ: ", 0

	;; jump table of operation codes
optab	.word op_all_complete-1, erase-1, erase_blk-1, program-1
ops	;; operations to be performed (terminated by 0)
;	.byte 1			; erase chip

;	.byte 2			; erase sector
;	.byte 0			; parameter => sector number (0..63, A21..A16)

;	.byte 3			; write data
;	.word image_end-image_start
;	.byte 0, 0, 0		; at this address (little endian)
;#include "content.s"		; the data (defines image_start and image_end)
;	.byte 0			; end of operations
