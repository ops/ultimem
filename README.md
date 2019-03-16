# Vic UltiMem software

The UltiMem is an "all-in-one" cartridge for the Commodore Vic-20 that
consists of in-system programmable Flash ROM and RAM, enough for
storing and running almost all Vic-20 software ever produced.  The
hardware is a product of RETRO Innovations, by Jim Brain:
http://go4retro.com/products/ultimem/
http://store.go4retro.com/

The UltiMem is a similar to the Vic Flash Plugin (4MiB flash and 32KiB
RAM), of which only some prototypes were built.  It comes in two
variants: VICMIDI with 512KiB+128KiB (with 256KiB or 512KiB RAM options)
and 8MiB+1MiB.

## Installation

You can try the UltiMem in VICE, starting with r30371, as follows.

	xvic -ultimem ultimem512k-s.img
	xvic -ultimem ultimem8m-s.img

The files are sample menu images generated by makecart according to
the file prg.txt.  You can generate a more comprehensive menu image from
filelist.txt.  It expects to find a collection of Vic-20 software in
the "vic20" subdirectory.  Invoke it as follows:

	make ultimem8m.img
	xvic -ultimem ultimem8m.img

or (for real hardware)

	make flash4file.prg ultimem8m.bin

Copy the files flash4file.prg and ultimem8m.bin to a medium that is
accessible from a Vic-20 that is equipped with the UltiMem.  Start the
Vic-20 in unexpanded memory configuration.

### Enabling a Fastloader

Because flash4file.prg uses the KERNAL file routines for loading the
data, it is slow.  The good news is that fastloaders should work.  If
your storage device implements the JiffyDOS protocol but your Vic-20
is not equipped with JiffyDOS, you can load an external fastloader
prior to loading and starting flash4file.prg.  The SJLOAD-20 release
07 comes in two precompiled variants, both residing in extended
memory.  Because the UltiMem starts up in unexpanded configuration by
default, you will have to change the memory configuration first:

	LOAD"ULTIMEM.PRG",8,1
	RUN

Press f5 and f7, then

	LOAD"SJLOAD$B0.PRG",8,1
	NEW
	SYS45056

Alternatively, press f5, 4, and f7, then

	LOAD"SJLOAD$04.PRG",8,1
	SYS1024

Now that the fastloader is installed, you can proceed to the next step.

### Installation from a File

Load the loader:

	LOAD"FLASH4FILE.PRG",8,1
	LIST

You should see one line:

	2016 SYS4130"ULTIMEM.BIN",00:::::

Invoke the program by typing the SYS statement.  The first parameter
is the image file name, and the second one is the device number (0 to
use the last accessed device).  You can also edit this line in place
(avoid changing the length of the line!) and type RUN.

	SYS4130"ULTIMEM8M.BIN",8

The UltiMem LED will flash while the operation is in progress.  You
may want to turn off the screen.  Even with SJLOAD-20 and sd2iec, it
may take 30 to 60 minutes to program the entire 8MiB flash.

The flash file system format is different from the Vic Flash Plugin.
Files are no longer compressed, and ROM images can be stored in 8KiB
blocks on their own, so that ROM cartridge images can be switched
quickly, without involving any copying.

## Features

The RAM expansion type can be selected programmatically.  Flash or RAM
can be mapped at any of the expansion areas: RAM[123] ($0400-$0fff),
BLK1 ($2000-$3fff), BLK2 ($4000-$5fff), BLK3 ($6000-$7fff), BLK5
($a000-$bfff), I/O2 ($9800-$9bff), I/O3 ($9c00-$9fff).  For each block,
there are four possible settings: unmapped, RAM, write-protected RAM,
and flash.

The register file is mapped at $9ff0-$9fff.  For maximum compatibility,
it can be disabled until reset or a special re-enabling sequence.

There are two push-button switches and one LED attached to one
register.  The third switch on the cartridge (the rightmost one when
viewed from the front of the keyboard) is a reset button.  Holding the
middle button while pressing the right button (reset) will unmap the
flash from memory, which is useful if a non-working image was written.

## Software

All software is licensed under the GNU General Public License, either
version 2 (or at your option) any later version of the license.

The machine language software has been translated with the XA (xa65)
cross-assembler by Andre Fachat and others.

| makecart, makecart.c | Cartridge image generator software. Sample source files are in prg.txt and filelist.txt. |
| flash4file, flash4file.c | Generates a flash programming image file for flash4.s out of a 4M image generated by makecart. |
| ramtest.s, ramtest.prg |
	RAM test. Starts in the unexpanded configuration. |
| flash.s | Source code for flash*.prg. |
| flash4file.prg | Copies data from a file (default "ultimem.bin") to Flash ROM. The file is to be generated like this:
	"flash4file menu-s.bin flash.bin".
	You may change the file name on the SYS line, provided that you do
	not change the length of the line. The program can be rerun. |
| flash8m.prg |	A stand-alone program that copies the sample image ultimem8m-s.bin
	to the flash memory. This can be used for testing the hardware.
	Requires a 24K memory configuration (POKE40946,63:SYS64802). |
| flash512k.prg |
	A stand-alone program that copies the sample image ultimem512k-s.bin
	to the flash memory. This can be used for testing the hardware.
	Requires a 24K memory configuration (POKE40946,63:SYS64802). |
| menu.s | The menu software. |
| ultimem.s, ultimem.prg | 
	Quick reference for using the menu software, and a memory
	configuration selector.
	Press f1 or f3 to read instructions, f5 to select RAM configurations,
	and f7 to exit.
	In the menu images that can be generated from the Makefile, this
	program is accessible by pressing f1, or by pressing RETURN when
	the cursor is positioned on the first menu item. |

The menu software automatically selects the RAM configuration for
program images.  The selection can be influenced at image creation time
by writing a '3' or '8' immediately after the closing quote.  See
makecart.c, and see filelist.txt for examples.  In this way, a
cartridge image can be bundled with a RAM expansion.

By holding a key during hardware reset or pressing a key in the menu,
you can enter the Commodore BASIC interpreter in different memory
configurations:

key 	memory configuration
STOP 	unexpanded (3583 bytes free)
3 	3 KiB memory expansion (6655 bytes free)
8 	24 KiB memory expansion (28159 bytes free)
	(compatible with 8 KiB and 16 KiB)

If you additionally hold the SHIFT key, the configuration registers
will be hidden, for maximum compatibility with programs that write to
normally unmapped address space ($9ff0-$9fff).

The menu software is accessed by the keyboard as follows.

key	 	memory configuration
a-z 		jump to the next item starting with the letter
A-Z 		jump to the previous item starting with the letter
CRSR UP		line up
CRSR DOWN	line down
CRSR LEFT	page up
CRSR RIGHT	page down
HOME 		first item
SHIFT+HOME	last item
RETURN 		activate the selected item
SHIFT+RETURN 	activate the selected item, hiding the configuration registers
f1 to f8	hotkeys for activating menu items

		In the images generated from the Makefile:

f1		Start ultimem.prg (quick reference, RAM config selection)
f5		Start ramtest.prg (RAM test, long)
f7		Start banktest.prg (bank register test)
f8		Start flash4file.prg (Flash memory programmer)

## Version history

Version 1.0.1, 2016-07-11

* ultimem.prg: Change the UltiMem logo colors
* flash.s: Indicate with the LED when an operation is in progress.
* flash4file.prg: Preserve the BLK1,BLK2,BLK5 memory configuration.
* The "Enabling a Fastloader" section in these instructions.

Version 1.0, 2016-06-24
Initial public release

## Acknowledgements

I would like to express my thanks to the following people:

Jim Brain for

* warming up the old idea
* developing the hardware and VHDL
* sample hardware for testing

The VICE team for

* perfecting VICE so that it runs the menu software without glitches
* "reference implementations" of bank-switched memory expansions
* applying my patches

my sons for

* showing interest in retro computing
* motivating me to complete this project

my wife Heli for

* bearing said sons
* bearing with me while I worked on this project

Marko Makela
http://www.iki.fi/~msmakela/8bit/ultimem/
