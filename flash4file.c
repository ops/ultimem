/** @file flash4file.c
Flash programming file generator
@author Marko Mäkelä (marko.makela@iki.fi) */

/* Copyright © 2003,2010-2012,2015-2016 Marko Mäkelä (marko.makela@iki.fi)

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License along
with this program; if not, write to the Free Software Foundation, Inc.,
51 Franklin Street, Suite 500, Boston, MA 02110-1335 USA. */

#include<stdio.h>
#include<string.h>

/** Operation codes for flash programming */
enum flash_op {
  /** end of file */
  FLASH_EOF = 0,
  /** erase chip */
  FLASH_ERASE_CHIP,
  /** erase sector (parameter = sector number) */
  FLASH_ERASE_BLK,
  /** write data (16 bits length, 24 bits address) */
  FLASH_PROGRAM
};

/** The main program
 * @param argc	number of command-line arguments
 * @param argv	the command line arguments
 */
int
main (int argc, char** argv)
{
  FILE* cart;
  FILE* flash;
  size_t blksz;
  size_t ofs;
  long pos = 0;

  if (argc != 3) {
    fputs ("usage: ", stderr);
    fputs (*argv, stderr);
    fputs (" cart.bin flash.bin\n", stderr);
    return 1;
  }

  if (!(cart = fopen (argv[1], "r+b"))) {
    fputs (argv[1], stderr);
    perror (": fopen (read)");
    return 2;
  }

  if (!(flash = fopen (argv[2], "w+b"))) {
    fputs (argv[2], stderr);
    perror (": fopen (write)");
    fclose (cart);
    return 2;
  }

  ofs = blksz = 0;

  for (;;) {
    int c = getc (cart);
  gotc:
    switch (c) {
    case 0xff:
      if (pos == 0) {
	if (++ofs < 16 << 20)
	  continue;
	break;
      }
      c = getc (cart);
      ungetc (c, cart);
      if (c == 0xff || c == EOF) {
	c = 0xff;
	goto end_of_block;
      }
      c = 0xff;
    default:
      if (pos == 0) {
	if (!ftell (flash))
	  putc (FLASH_ERASE_CHIP, flash); /* start by erasing the chip */
	putc (FLASH_PROGRAM, flash);
	pos = ftell (flash);
	putc (0, flash), putc (0, flash); /* length */
	putc (ofs & 0xff, flash);
	putc ((ofs >> 8) & 0xff, flash);
	putc (ofs >> 16, flash);
      }

      putc (c, flash);

      if (++blksz < 65535)
	break;
      /* fall through if the block grows to 64K in length */
    case EOF:
    end_of_block:
      if (pos) {
	if (fseek (flash, pos, SEEK_SET)) {
	seek_error:
	  fputs (argv[2], stderr);
	  perror (": fseek");
	  fclose (cart);
	  fclose (flash);
	  return 3;
	}

	if (putc (blksz & 0xff, flash) == EOF ||
	    putc (blksz >> 8, flash) == EOF) {
	  fputs (argv[2], stderr);
	  perror (": putc");
	  fclose (cart);
	  fclose (flash);
	  return 3;
	}

	if (fseek (flash, 0, SEEK_END))
	  goto seek_error;
      }
      pos = 0;
      blksz = 0;
    }

    if (c == EOF)
      break;
    if (++ofs < 16 << 20)
      continue;
    c = EOF;
    goto gotc;
  }

  fprintf (stderr, "%s: read %u bytes\n", argv[1], (unsigned) ofs);
  putc (FLASH_EOF, flash);
  fclose (cart);
  fclose (flash);
  return 0;
}
