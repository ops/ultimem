/** @file makecart.c
Vic UltiMem flash image generator
@author Marko Mäkelä (marko.makela@iki.fi) */

/* Copyright © 2003,2010,2011,2015,2016 Marko Mäkelä (marko.makela@iki.fi)

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

#include<assert.h>
#include<search.h>
#include<stdio.h>
#include<string.h>
#include<stdlib.h>
#include<stdint.h>
#include<ctype.h>
#include"readline.h"

typedef uint_fast32_t fladdr_t;
typedef uint_fast16_t word_t;
typedef uint_fast8_t byte_t;
typedef uint8_t octet_t;

/** Directory entry type flags */
enum typeflags
{
  /** BASIC program */
  mBASIC = 0x4f,
  /** Cartridge image (the low nibble indicates the number of 8k blocks) */
  mCartridge = 0x8f,
  /** Subdirectory */
  mDirectory = 0xff
};
/** Type of memory expansion */
enum memflags
{
  /** 3k memory expansion */
  m3k = 0x10,
  /** 8k (up to 24k) memory expansion */
  m8k = 0x20,
  /** No memory expansion, or subdirectory */
  m0k = 0x30
};

/** The name part of a menu entry */
typedef octet_t name_t[16];

/** Signifies an unknown memory address */
enum { ADDR_UNKNOWN = (1 << 24) - 1 };

/** Menu */
struct menu
{
  /** NULL-terminated singly linked list of unresolved references to
  the menu (NULL if the menu has been resolved) */
  struct menu* next;
  /** Line number of the reference or definition */
  unsigned lineno;
  /** If next==NULL, start offset, or ADDR_UNKNOWN if the menu is unresolved.
  If next != NULL, a menu referring to the entry. */
  fladdr_t addr;
};
/** Menu definition or reference */
struct menuref
{
  /** Menu label */
  char* name;
  /** The menu definition, or unresolved references to the menu */
  struct menu* data;
};

/** Menu entry */
struct menuent
{
  /** Previous entry */
  octet_t prev[3];
  /** Next entry */
  octet_t next[3];
  /** Flags */
  octet_t flags;
  /** Entry text */
  name_t name;
};

/** The cartridge image. */
static octet_t* image;
/** offset of the allocation bitmap */
#define bitmap_offset 0x10000
/** size of the allocation bitmap */
#define bitmap_size 256
/** the function key shortcuts: f1,f3,f5,f7,f2,f4,f6,f8 */
#define fn_offset bitmap_offset + bitmap_size + 3
/** start of the root menu */
#define rootmenu_offset fn_offset + 3 * 8

/** Map a function key to a fn_offset[] slot
@param key function key ('1' to '8')
@return reference to fn_offset */
static fladdr_t
fnaddr(char key)
{
  assert (key >= '1');
  assert (key <= '8');
  unsigned k = key - '1';
  k = k >> 1 | (k & 1) << 2;
  assert (k < 8);
  return fn_offset + 3 * k;
}

/** first and last available byte on cartridge */
static fladdr_t firstAddr;
/** address of previous menu entry */
static fladdr_t menuPrev;

/** start and end address of the program */
static uint_fast16_t prgStart, prgEnd;
/** program buffer */
static uint8_t prgData[65536];
/** warp page of the program */
static uint_fast16_t prgWarp;
/** entry type flags */
static enum typeflags prgTypeFlags;
/** memory type flags */
static enum memflags prgMemFlags;

/** Allocate a 8KiB block downwards.
 * @return start address
 * @retval 0 on failure
 */
static fladdr_t
allocate8K (void)
{
  unsigned i, j;

  for (i = bitmap_size; i--; ) {
    octet_t* a = &image[bitmap_offset + i];
    octet_t b;
    for (j = 8, b = 1; j--; b <<= 1) {
      if (*a & b) {
        *a &= ~b;
        return i << 16 | j << 13;
      }
    }
  }

  return 0;
}

/** Allocate memory upwards.
 * @param addr	start address
 * @param size	bytes to reserve
 * @return 0 on success, nonzero on failure
 */
static int
allocate (fladdr_t addr, size_t size)
{
  unsigned i0 = addr >> 16, j0 = 7 & (addr >> 13);
  unsigned i1 = (addr + size - 1) >> 16, j1 = 7 & ((addr + size - 1) >> 13);
  unsigned j2 = (i0 == i1) ? j1 : 7;
  unsigned i, j, k = 0;
  octet_t* a, b, allocation[bitmap_size];
  memcpy (allocation, image + bitmap_offset, bitmap_size);

  assert (size > 0);

  a = &allocation[i0];
  for (j = j0, b = 1 << (7 - j); j <= j2; b >>= 1, j++) {
    if (*a & b) {
      *a &= ~b;
      k++;
    }
    else if (j != j0)
      return 1;
  }

  for (i = i0 + 1; i < i1; i++, k += 8) {
    if (allocation[i] != 0xff)
      return 2;
    allocation[i] = 0;
  }

  if (i0 != i1) {
    a = &allocation[i1];

    for (j = 0, b = 1 << 7; j <= j1; b >>= 1, j++, k++) {
      if (!(*a & b))
        return 3;
      *a &= ~b;
    }
  }

  if (k == 0)
    return 0;

  memcpy (image + bitmap_offset, allocation, bitmap_size);
  return 0;
}

/** Encode an address
 * @param buf	buffer of 3 octets
 * @param addr	the address
 */
static void
encodeAddr (octet_t* buf, fladdr_t addr)
{
  assert (addr < (1 << 24));

  buf[0] = addr;
  buf[1] = addr >> 8;
  buf[2] = addr >> 16;
}

/** Update an address in a menu entry.
 * @param dest	where to write the address
 * @param addr	the address to be written
 */
static void
updateOffset (fladdr_t dest, fladdr_t addr)
{
  encodeAddr (image + dest, addr);
}

/** Write a submenu reference
 * @param ent		the menu item
 * @param addr		the submenu offset (ADDR_UNKNOWN if unknown)
 * @param lineno	input line number for diagnostics
 * @return	offset of the reference, ADDR_UNKNOWN on failure
 */
static fladdr_t
writeSubmenu (struct menuent* ent, fladdr_t addr, unsigned lineno)
{
  const size_t size = 3 + sizeof *ent;
  fladdr_t nextAddr = firstAddr + size;
  octet_t* e = image + firstAddr;

  if (allocate (firstAddr, size)) {
    fprintf (stderr, "%u: no space left in menu\n", lineno);
    return ADDR_UNKNOWN;
  }

  encodeAddr (ent->prev, menuPrev);
  encodeAddr (ent->next, ADDR_UNKNOWN);
  ent->flags = mDirectory;

  /* Adjust the previous menu entry pointer */
  if (menuPrev != ADDR_UNKNOWN)
    updateOffset (menuPrev + 3, firstAddr);

  memcpy (e, ent, sizeof *ent);
  e += sizeof *ent;
  *e++ = (octet_t) addr;
  *e++ = (octet_t) (addr >>= 8);
  *e++ = (octet_t) (addr >>= 8);

  menuPrev = firstAddr;
  firstAddr = nextAddr;
  return nextAddr - 3;
}

/** Copy a ROM image to a page.
 * @param data		data buffer
 * @param size		size of the data, in bytes
 * @param start		start address of the data
 * @param addr		address of the block (2 bytes)
 * @param lineno	input line number for diagnostics
 * @return		0 on success, nonzero on failure
 */
static int
copyImage (const uint8_t* data, unsigned size, uint_fast16_t start,
	   uint8_t* addr, unsigned lineno)
{
  uint8_t page[8192];
  fladdr_t dest;

  if (size <= 0x200 || size > sizeof page) {
  size:
    fprintf (stderr,
	     "%u: unsupported cartridge image: size %u,"
	     " start address 0x%04x\n",
	     lineno, size, (unsigned) start);
    return -1;
  }

  memcpy (page, data, size);

  /* Pad the data to a power-of-2 size. */
  while (size & (size - 1))
    page[size++] = 0xff;

  if (start & (size - 1))
    goto size;

  switch (start & 0xe000) {
  default:
    fprintf (stderr, "%u: unsupported start address 0x%04x\n",
	     lineno, (unsigned) start);
    return -1;
  case 0x2000: addr[1] = 0 << 5 | 0x18; break;
  case 0x4000: addr[1] = 1 << 5 | 0x18; break;
  case 0x6000: addr[1] = 2 << 5 | 0x18; break;
  case 0xa000: addr[1] = 3 << 5 | 0x18; break;
    break;
  }

  switch (size) {
  default:
    assert (!"invalid size");
    goto size;
  case 0x400:
    memcpy (page + 0x400, data, size);
    memcpy (page + 0xc00, data, size);
    memcpy (page + 0x1400, data, size);
    memcpy (page + 0x1c00, data, size);
  case 0x800:
    memcpy (page + 0x800, data, size);
    memcpy (page + 0x1800, data, size);
  case 0x1000:
    memcpy (page + 0x1000, data, size);
  case 0x2000:
    break;
  }

  dest = allocate8K ();

  if (dest == 0) {
    fprintf (stderr, "%u: out of memory for cartridge images\n", lineno);
    return -1;
  }

  addr[0] = dest >> 13;
  addr[1] |= dest >> 21;

  memcpy (image + dest, page, sizeof page);
  return 0;
}

/** Close a menu entry.
 * @param lineno	input line number for diagnostics
 * @param ent		the menu item
 * @return		0 on success, nonzero on failure
 */
static int
closeEntry (unsigned lineno, struct menuent* ent)
{
  uint8_t addrs[8];
  unsigned addrsize = 0;
  uint_fast16_t size;
  uint_fast16_t start = prgStart, end = prgEnd, warp = prgWarp;

  prgStart = prgEnd = 0;
  prgWarp = 0x8000;

  encodeAddr (ent->prev, menuPrev);
  encodeAddr (ent->next, ADDR_UNKNOWN);
  ent->flags = prgTypeFlags | prgMemFlags;

  if (prgTypeFlags == mCartridge) {
    const uint8_t* data = prgData;
    size = 0;
    for (;;) {
      const uint_fast16_t bend = (start & ~8191) + 8192;
      const uint_fast16_t blen = ((end > bend)
				  ? ((warp > bend) ? bend : warp)
				  : end) - start;
      if (copyImage (data, blen, start, addrs + addrsize, lineno))
	return -1;
      data += blen;
      start += blen;
      addrsize += 2;
      assert (start == end || start == warp || !(start & 8191));
      assert (start <= end);
      if (start >= end)
	break;
      if (start == warp) {
	start = 0xa000;
	end += start - warp;
      }
      ent->flags--;
    }
  }
  else {
    size = end - start;
    addrs[0] = start;
    addrs[1] = start >> 8;
    addrs[2] = end;
    addrs[3] = end >> 8;
    addrsize = 4;
  }

  if (allocate (firstAddr, size + addrsize + sizeof *ent)) {
    fprintf (stderr, "%u: no space left in image\n", lineno);
    return -1;
  }
  else {
    octet_t* e = image + firstAddr;

    if (menuPrev != ADDR_UNKNOWN)
      updateOffset (menuPrev + 3, firstAddr);

    memcpy (e, ent, sizeof *ent);
    e += sizeof *ent;
    memcpy (e, addrs, addrsize);
    e += addrsize;
    memcpy (e, prgData, size);

    menuPrev = firstAddr;
    firstAddr += size + addrsize + sizeof *ent;
    return 0;
  }
}

/** Add a file to a menu entry
 * @param name		name of the file
 * @param lineno	input line number for diagnostics
 * @param flags		file flags
 * @param ent		the menu entry
 * @return		ent on success, NULL on failure
 */
static struct menuent*
addFile (const char* name,
	 unsigned lineno,
	 struct menuent* ent)
{
  /** length of the block read */
  uint_fast16_t length;
  /** room for start address */
  uint8_t start[2];
  /** the file */
  FILE* f = fopen (name, "rb");
  if (!f) {
    fprintf (stderr, "%u: %s: ", lineno, name);
    perror ("fopen");
    return 0;
  }
  if (2 != fread (start, 1, sizeof start, f)) {
    fprintf (stderr, "%u: %s: ", lineno, name);
    perror ("fread(start)");
    fclose (f);
    return 0;
  }
  length = prgEnd - prgStart;
  length = fread (prgData + length, 1, (sizeof prgData) - length, f);
  if (!length || ferror (f)) {
    fclose (f);
    fprintf (stderr, "%u: %s: ", lineno, name);
    perror ("fread(data)");
    return 0;
  }

  fclose (f);

  if (!prgStart && !prgEnd) {
    /* first file */
    prgStart = start[0];
    prgStart |= start[1] << 8;

    switch (prgStart) {
    case 0x0401: prgMemFlags = m3k; break;
    case 0x1001: prgMemFlags = m0k; break;
    case 0x1201: prgMemFlags = m8k; break;
    case 0x2000: case 0x3000: case 0x4000: case 0x5000:
    case 0x6000: case 0x7000: case 0xa000: case 0xb000:
      prgTypeFlags = mCartridge;
      break;
    default:
      fprintf (stderr, "%u: %s: unsupported start address 0x%04x\n",
	       lineno, name, (unsigned) prgStart);
      prgStart = 0;
      return 0;
    }

    prgEnd = length + prgStart;
    prgWarp = prgTypeFlags == mCartridge && prgEnd < 0x8000
      ? prgEnd
      : 0x8000;
    return ent;
  }
  else if ((!start[0] && start[1] == (uint8_t) 0xa0 && prgEnd == prgWarp) ||
	   prgEnd == (((uint_fast16_t) start[1] << 8) | start[0])) {
    /* subsequent file */
    prgEnd += length;
    return ent;
  }
  else {
    fprintf (stderr,
	     "%u: %s: starts at 0x%04x, not 0x%04x\n",
	     lineno, name,
	     (unsigned) (((uint_fast16_t) start[1] << 8) | start[0]),
	     (unsigned) prgEnd);
    prgStart = prgEnd = 0;
    return 0;
  }
}

/** Construct a menu name
 * @param str	name as a null-terminated character string
 * @param name	(output) the menu entry name
 */
static void
mkname (const char* str,
	octet_t* name)
{
  size_t len;
  const char* s = str;

  for (len = 0; len < sizeof (name_t); len++) {
    char c = *s++;
    if (!c)
      break;
    if (c >= 'a' && c <= 'z')
      c -= 'a' - 0x41;
    else if (c >= 'A' && c <= 'Z')
      c -= 'A' - 0xc1;
    else if (c == '\\') {
      c = *s++;
      switch (c) {
	unsigned i;
      case 'a': c = 0x07; break;
      case 'b': c = 0x08; break;
      case 't': c = 0x09; break;
      case 'n': c = 0x0a; break;
      case 'v': c = 0x0b; break;
      case 'f': c = 0x0c; break;
      case 'r': c = 0x0d; break;
      case 'x':
	/* convert up to 2 hexadecimal digits to a byte */
	c = 0;
	for (i = 0;
	     i < 2 && ((*s >= '0' && *s <= '9') ||
		       (*s >= 'a' && *s <= 'f') ||
		       (*s >= 'A' && *s <= 'F'));
	     i++) {
	  char d = *s++;
	  if (d >= '0' && d <= '9')
	    d -= '0';
	  else if (d >= 'a' && d <= 'f')
	    d -= 'a' - 0xa;
	  else if (d >= 'A' && d <= 'F')
	    d -= 'A' - 0xa;
	  c <<= 4;
	  c |= d;
	}
	if (i == 0)
	  c = 'x'; /* no digits given: translate \x to x */
	break;
      default:
	/* convert up to 3 octal digits to a byte */
	s--;
	c = 0;
	for (i = 0; *s >= '0' && *s <= '7' && i < 3; i++) {
	  c <<= 3;
	  c |= *s++ - '0';
	  if (c >= 040)
	    break; /* prevent overflow */
	}
	if (i == 0)
	  c = *s; /* no digits given: omit the \ */
	break;
      }
    }
    name[len] = c;
  }

  /* Pad with spaces */
  memset (name + len, 0x20, sizeof (name_t) - len);
}

/** Compare two menu references
 * @param a	pointer to struct menuref
 * @param b	pointer to struct menuref
 * @return	0 if a and b are equal, nonzero if not
 */
static int
refcmp (const void* a,
	const void* b)
{
  return strcmp (((const struct menuref*) a)->name,
		 ((const struct menuref*) b)->name);
}

/** Process a menu specification
 * @param file	the input stream
 */
static void
processFile (FILE* file)
{
  char* line;
  unsigned lineno;
  struct menuent menu;
  /** Menu references or labels */
  struct menuref* ref = NULL;
  /** Number of entries in ref */
  size_t n_ref = 0;
  /** Old number of entries in ref
  (for detecting if lsearch entered the key) */
  size_t n_ref_old;
  /** Allocated size of ref */
  size_t ref_size = 0;

  /** menu status */
  enum {
    /** closed: menu not being constructed */
    mClosed,
    /** menu opened */
    mOpened,
    /** created: menu entry given a name but not contents */
    mCreated,
    /** populated: menu entry given a name and (one) file */
    mPopulated
  } menuStatus = mClosed;

  memset (&menu, 0, sizeof menu);

  for (lineno = 1; (line = readline (file)); lineno++) {
    struct menuref* submenu;
    struct menuref submenuref;
    char* l;

    l = strchr (line, '#');
    if (l)
      *l = 0; /* # starts a comment until the end of the line */
    while (l-- > line && isspace (*l))
      *l = 0; /* discard trailing whitespace */
    l = line;
  nextChar:
    if (*l == '(') { /* opening a menu definition */
      switch (menuStatus) {
      case mPopulated:
	if (closeEntry (lineno, &menu))
	  fprintf (stderr, "%u: no space left in menu\n", lineno);
	/* fall through */
      closeMenu:
      case mOpened:
	fprintf (stderr, "%u: closing previous menu definition\n", lineno);
	break;
      case mCreated:
	fprintf (stderr, "%u: previous menu entry is is empty\n", lineno);
	goto closeMenu;
      case mClosed:
	break;
      }

      menuStatus = mOpened;
      menuPrev = ADDR_UNKNOWN;
      l++;
      goto nextChar;
    }
    else if (*l == ')') { /* closing a menu definition */
      switch (menuStatus) {
      case mPopulated:
	if (closeEntry (lineno, &menu))
	  fprintf (stderr, "%u: no space left in menu\n", lineno);
	/* fall through */
      case mOpened:
	break;
      case mCreated:
	fprintf (stderr, "%u: last menu entry is is empty\n", lineno);
	break;
      case mClosed:
	fprintf (stderr, "%u: no menu is open\n", lineno);
	break;
      }

      menuStatus = mClosed;
      menuPrev = ADDR_UNKNOWN;
      l++;
      goto nextChar;
    }
    else if (*l == '*') { /* define a menu label: *menu_ref */
      switch (menuStatus) {
      case mPopulated:
      case mOpened:
      case mCreated:
	break;
      case mClosed:
	fprintf (stderr, "%u: no menu is open\n", lineno);
	goto nextLine;
      }

      if (n_ref >= ref_size) {
	size_t newsize = ref_size;
	if (newsize) {
	  struct menuref* newref;
	  do newsize <<= 1; while (n_ref >= newsize);
	  newref = realloc (ref, newsize * sizeof *ref);
	  if (!newref)
	    goto oomlabel;
	  ref_size = newsize;
	  ref = newref;
	}
	else {
	  newsize = 1;
	  ref = malloc (newsize * sizeof *ref);
	  if (!ref) {
	  oomlabel:
	    fprintf (stderr, "%u: out of memory for menu label\n", lineno);
	    goto nextLine;
	  }
	  ref_size = newsize;
	}
      }

      submenuref.name = ++l;
      n_ref_old = n_ref;
      submenu = lsearch (&submenuref, ref, &n_ref, sizeof *submenu, refcmp);
      if (n_ref == n_ref_old) {
	/* Label found: Resolve deferred references to this menu. */
	struct menu* data = submenu->data;
	while (data->next) {
	  struct menu* next;

	  updateOffset (data->addr, firstAddr);

	  next = data->next;
	  free (data);
	  data = next;
	}
	/* Replace the entry. */
	data->lineno = lineno;
	data->addr = firstAddr;
	data->next = NULL;
	submenu->data = data;
      }
      else {
	/* Enter the menu. */
	submenu->name = strdup (submenu->name);
	submenu->data = malloc (sizeof *submenu->data);
	if (!submenu->name || !submenu->data) {
	  n_ref--;
	  free (submenu->name);
	  goto oomlabel;
	}
	submenu->data->next = NULL;
	submenu->data->lineno = lineno;
	submenu->data->addr = firstAddr;
      }

      goto nextLine;
    }
    else if (*l == 'f' && l[1] >= '1' && l[1] <= '8'
	     && l[2] == '\0') { /* f1 to f8: hotkey */
      updateOffset (fnaddr(l[1]), firstAddr);
      goto nextLine;
    }
    else if (*l == '"') { /* "name" [3|8] filenames or "name"/menu_ref */
      char* name = l + 1;
      char* end = strchr (name, '"');
      if (end) {
	*end = 0;

	switch (menuStatus) {
	case mClosed:
	  fprintf (stderr, "%u: opening menu for \"%s\"\n",
		   lineno, name);
	  menuStatus = mOpened;
	  menuPrev = ADDR_UNKNOWN;
	  break;
	case mOpened:
	  break;
	case mCreated:
	  fprintf (stderr, "%u: predecessor of \"%s\" is empty\n",
		   lineno, name);
	  break;
	case mPopulated:
	  if (closeEntry (lineno, &menu))
	    fprintf (stderr, "%u: no space left in menu\n", lineno);
	  break;
	}

	mkname (name, menu.name);
	menuStatus = mCreated;

	l = end + 1;
	switch (*l) {
	default:
	  prgMemFlags = m0k;
	  break;
	case '3':
	  prgMemFlags = m3k, l++;
	  break;
	case '8':
	  prgMemFlags = m8k, l++;
	  break;
	case '/':
	  if (n_ref >= ref_size) {
	    unsigned newsize = ref_size;
	    if (newsize) {
	      struct menuref* newref;
	      do newsize <<= 1; while (n_ref >= newsize);
	      newref = realloc (ref, newsize * sizeof *ref);
	      if (!newref)
		goto oomref;
	      ref_size = newsize;
	      ref = newref;
	    }
	    else {
	      newsize = 1;
	      ref = malloc (newsize * sizeof *ref);
	      if (!ref) {
	      oomref:
		fprintf (stderr, "%u: out of memory for menu reference\n",
			 lineno);
		goto nextLine;
	      }
	      ref_size = newsize;
	    }
	  }

	  submenuref.name = ++l;
	  n_ref_old = n_ref;
	  submenu = lsearch (&submenuref, ref, &n_ref, sizeof *submenu, refcmp);
	  if (n_ref_old != n_ref) {
	    /* First unresolved menu reference */
	    submenu->name = strdup (submenu->name);
	    submenu->data = malloc (sizeof *submenu->data);
	    if (!submenu->name || !submenu->data) {
	    oomref2:
	      n_ref--;
	      free (submenu->name);
	      free (submenu->data);
	      goto oomref;
	    }
	    submenu->data->next = calloc (1, sizeof *submenu->data->next);
	    if (!submenu->data->next)
	      goto oomref2;

	    submenu->data->lineno = lineno;
	    submenu->data->addr = writeSubmenu (&menu, ADDR_UNKNOWN, lineno);
	    if (submenu->data->addr == ADDR_UNKNOWN) {
	      free (submenu->data->next);
	      free (submenu->name);
	      free (submenu->data);
	      n_ref--;
	      goto unresolvedMenuRef;
	    }
	  }
	  else if (submenu->data->next) {
	    /* Defer an unresolved menu reference */
	    struct menu* data = malloc (sizeof *data);
	    if (!data)
	      goto oomref;
	    data->lineno = lineno;
	    data->addr = writeSubmenu (&menu, ADDR_UNKNOWN, lineno);
	    if (data->addr == ADDR_UNKNOWN) {
	      free (data);
	    unresolvedMenuRef:
	      fprintf (stderr, "%u: unresolvable submenu reference: ", lineno);
	      goto nextLine;
	    }

	    data->next = submenu->data->next;
	    submenu->data = data;
	  }
	  else
	    /* Resolved menu reference */
	    writeSubmenu (&menu, ((struct menu*) submenu->data)->addr, lineno);
	  menuStatus = mOpened;
	  goto nextLine;
	}
	prgTypeFlags = mBASIC;

	if (isspace (*l))
	  goto addFile;
      }
      else
	fprintf (stderr, "%u: unterminated '\"'\n", lineno);
    }
    else if (isspace (*l)) {
    addFile:
      while (isspace (*l))
	l++;
      if (*l) {
	switch (menuStatus) {
	case mClosed:
	case mOpened:
	  fprintf (stderr, "%u: no menu specified\n", lineno);
	  break;
	case mPopulated:
	  if (addFile (l, lineno, &menu) && !closeEntry (lineno, &menu))
	    menuStatus = mOpened;
	  else {
	    fprintf (stderr, "%u: no space left in menu\n", lineno);
	    free (line);
	    return;
	  }
	  break;
	case mCreated:
	  menuStatus = addFile (l, lineno, &menu) ? mPopulated : mOpened;
	  break;
	}
      }
      goto nextLine;
    }
    while (isspace (*l))
      l++;
    if (*l)
      fprintf (stderr, "%u: ignoring the rest of the line: %s\n",
	       lineno, l);
  nextLine:
    free (line);
  }
  switch (menuStatus) {
  case mClosed:
    break;
  case mPopulated:
    if (closeEntry (lineno, &menu))
      fprintf (stderr, "%u: no space left in menu\n", lineno);
    /* fall through */
  case mOpened:
    fprintf (stderr, "%u: last menu left open\n", lineno);
    break;
  case mCreated:
    fprintf (stderr, "%u: last menu entry left open\n", lineno);
    break;
  }

  if (ref) {
    unsigned i;
    /* Ensure that there are no unresolved menu references */

    for (i = 0; i < n_ref; i++) {
      struct menu* data = ref[i].data;
      while (data->next) {
	struct menu* next;
	fprintf (stderr, "%u: unresolved reference to menu %s\n",
		 data->lineno, ref[i].name);

	next = data->next;
	free (data);
	data = next;
      }
      free (data);
      free (ref[i].name);
    }

    free (ref);
  }
}

/** The main program
 * @param argc	number of command-line arguments
 * @param argv	the command line arguments
 */
int
main (int argc, char** argv)
{
  unsigned long size;
  size_t menusize;
  char* endp;
  FILE* f = NULL;

  if (argc != 4) {
  usage:
    fprintf (stderr, "usage: %s menu.prg output.img SIZE < filelist.txt\n",
	     *argv);
    return 1;
  }

  size = strtoul(argv[3], &endp, 0);
  if (*endp || size < 131072UL || size > (1UL << 24) || (size & (size - 1)))
    goto usage;

  image = malloc (size);
  if (image == NULL) {
    fprintf (stderr, "%s: cannot allocate %lu bytes\n", *argv, size);
    return 2;
  }

  f = fopen (argv[1], "rb");

  if (!f) {
    fputs (argv[1], stderr);
    perror (": fopen(reading)");
  error:
    free (image);
    return 2;
  }

  menusize = fread (image, 1, size, f);

  if (fclose (f)) {
    fprintf (stderr, "%s: closing %s", *argv, argv[1]);
    perror (": fclose");
    goto error;
  }

  if (menusize > 65536) {
    fprintf (stderr, "%s: %s does not look valid\n", *argv, argv[1]);
    goto error;
  }

  memset (image + menusize, 0xff, size - menusize);
  /* Initialize the allocation bitmap. */
  if (allocate (0, 65536 + 8192) ||
      (size != 1UL << 24 && allocate (size, (1UL << 24) - size))) {
    fprintf (stderr, "%s: unable to initialize the allocation for size %lu\n",
	     *argv, size);
    goto error;
  }

  /* Insert the payload. */
  firstAddr = rootmenu_offset;
  processFile (stdin);
  encodeAddr (&image[bitmap_offset + bitmap_size], firstAddr);

  f = fopen (argv[2], "wb");

  if (!f) {
    fputs (argv[2], stderr);
    perror (": fopen(writing)");
    goto error;
  }

  if (size != fwrite (image, 1, size, f)) {
    fputs (argv[2], stderr);
    perror (": fwrite");
    goto error;
  }

  if (fclose (f)) {
    fprintf (stderr, "%s: closing %s", *argv, argv[2]);
    perror (": fclose");
    goto error;
  }

  free (image);
  return 0;
}
