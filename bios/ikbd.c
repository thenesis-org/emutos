/*
 * ikbd.c - Intelligent keyboard routines
 *
 * Copyright (c) 2001 EmuTOS development team
 *
 * Authors:
 *  LVL   Laurent Vogel
 *  MAD   Martin Doering
 *
 * This file is distributed under the GPL, version 2 or at your
 * option any later version.  See doc/license.txt for details.
 */

/*
 * LVL: I rewrote this, taking bits from kbd.c and kbq.c,
 * to be more compliant with the iorec stuff.
 *
 * not supported: 
 * - mouse move using alt-arrowkeys
 * - alt-help screen hardcopy
 * - alt keys for non-us keyboards
 * - KEYTBL.TBL config with _AKP cookie (tos 5.00 and later)
 * - CLRHOME and INSERT in kbshift.
 */
 

#include "portab.h"
#include "bios.h"
#include "acia.h"
#include "kprint.h"
#include "tosvars.h"
#include "iorec.h"
#include "asm.h"
#include "ikbd.h"

#define DBG_KBD 0



/* scancode definitions */
#define KEY_RELEASED 0x80     /* This bit set, when key-release scancode */

#define KEY_LSHIFT  0x2a
#define KEY_RSHIFT  0x36
#define KEY_CTRL    0x1d
#define KEY_ALT     0x38
#define KEY_CAPS    0x3a


/*
 * These bit flags are set in the "modes" byte based on the state
 * of control keys on the keyboard, like:
 * right shift, left shift, control, alt, ...
 */
 
/* mode types - these are the different bits */
#define MODE_RSHIFT 0x01      /* right shift keys is down */
#define MODE_LSHIFT 0x02      /* right shift keys is down */
#define MODE_CTRL   0x04      /* CTRL is down.*/
#define MODE_ALT    0x08      /* ALT is down.			     */
#define MODE_CAPS   0x10      /* ALPHA LOCK is down. 		     */
#define MODE_CLEAR  0x20      /* CLR/HOME mode key is down    */

#define MODE_SHIFT   (MODE_RSHIFT|MODE_LSHIFT|MODE_CAPS)      /* shifted */



/*==== Global variables ===================================================*/
BYTE	shifty;          /* reflect the status up/down of mode keys */


/*==== Scancode table unshifted ===========================================*/
static BYTE ascii_norm [] = {
    0x00, 0x1b, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36,
    0x37, 0x38, 0x39, 0x30, 0x9e, 0x27, 0x08, 0x09,
    0x71, 0x77, 0x65, 0x72, 0x74, 0x7a, 0x75, 0x69,
    0x6f, 0x70, 0x81, 0x2b, 0x0d, 0x00, 0x61, 0x73,
    0x64, 0x66, 0x67, 0x68, 0x6a, 0x6b, 0x6c, 0x94,
    0x84, 0x23, 0x00, 0x7e, 0x79, 0x78, 0x63, 0x76,
    0x62, 0x6e, 0x6d, 0x2c, 0x2e, 0x2d, 0x00, 0x00,
    0x00, 0x20, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x2d, 0x00, 0x00, 0x00, 0x2b, 0x00,
    0x00, 0x00, 0x00, 0x7f, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x3c, 0x00, 0x00, 0x28, 0x29, 0x2f, 0x2a, 0x37,
    0x38, 0x39, 0x34, 0x35, 0x36, 0x31, 0x32, 0x33,
    0x30, 0x2e, 0x0d, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
};



/*==== Scancode table shifted =============================================*/
static BYTE ascii_shft [] = {
    0x00, 0x1b, 0x21, 0x22, 0xdd, 0x24, 0x25, 0x26,
    0x2f, 0x28, 0x29, 0x3d, 0x3f, 0x60, 0x08, 0x09,
    0x51, 0x57, 0x45, 0x52, 0x54, 0x5a, 0x55, 0x49,
    0x4f, 0x50, 0x9a, 0x2a, 0x0d, 0x00, 0x41, 0x53,
    0x44, 0x46, 0x47, 0x48, 0x4a, 0x4b, 0x4c, 0x99,
    0x8e, 0x5e, 0x00, 0x3e, 0x59, 0x58, 0x43, 0x56,
    0x42, 0x4e, 0x4d, 0x3b, 0x3a, 0x5f, 0x00, 0x00,
    0x00, 0x20, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x37,
    0x38, 0x00, 0x2d, 0x34, 0x00, 0x36, 0x2b, 0x00,
    0x32, 0x00, 0x30, 0x7f, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x3e, 0x00, 0x00, 0x28, 0x29, 0x2f, 0x2a, 0x37,
    0x38, 0x39, 0x34, 0x35, 0x36, 0x31, 0x32, 0x33,
    0x30, 0x2e, 0x0d, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
};

/*==== Scancode table with caps lock ======================================*/
static BYTE ascii_caps [] = {
    0x00, 0x1b, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36,
    0x37, 0x38, 0x39, 0x30, 0x9e, 0x27, 0x08, 0x09,
    0x51, 0x57, 0x45, 0x52, 0x54, 0x5a, 0x55, 0x49,
    0x4f, 0x50, 0x9a, 0x2b, 0x0d, 0x00, 0x41, 0x53,
    0x44, 0x46, 0x47, 0x48, 0x4a, 0x4b, 0x4c, 0x99,
    0x8e, 0x23, 0x00, 0x3c, 0x59, 0x58, 0x43, 0x56,
    0x42, 0x4e, 0x4d, 0x2c, 0x2e, 0x2d, 0x00, 0x00,
    0x00, 0x20, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x2D, 0x00, 0x00, 0x00, 0x2B, 0x00,
    0x00, 0x00, 0x00, 0x7F, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x3C, 0x00, 0x00, 0x28, 0x29, 0x2F, 0x2A, 0x37,
    0x38, 0x39, 0x34, 0x35, 0x36, 0x31, 0x32, 0x33,
    0x30, 0x2E, 0x0D, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
};

/*=== Keymaps handling (xbios) =======================================*/

static struct {
  BYTE *norm;
  BYTE *shft;
  BYTE *caps;
} key_map_table;

#define norm_key_map (key_map_table.norm)
#define shft_key_map (key_map_table.shft)
#define caps_key_map (key_map_table.caps)



LONG keytbl(LONG norm, LONG shft, LONG caps)
{
  if(norm) {
    norm_key_map = (BYTE *)norm;
  }
  if(shft) {
    shft_key_map = (BYTE *)shft;
  }
  if(caps) {
    caps_key_map = (BYTE *)caps;
  }
  return (LONG) &key_map_table;
}

VOID bioskeys(VOID)
{
  norm_key_map = ascii_norm;
  shft_key_map = ascii_shft;
  caps_key_map = ascii_caps;
}


/*=== iorec handling (bios) ==============================================*/

LONG bconstat2(VOID)
{
  if(ikbdiorec.head == ikbdiorec.tail) {
    return 0;   /* iorec empty */
  } else {
    return -1;  /* not empty => input available */
  }
}

LONG bconin2(VOID)
{
  WORD old_sr;
  LONG value;

  while(!bconstat2()) 
    ;
  /* disable interrupts */
  old_sr = set_sr(0x2700);
  
  ikbdiorec.head += 4;
  if(ikbdiorec.head >= ikbdiorec.size) {
    ikbdiorec.head = 0;
  }
  value = *(LONG *)(ikbdiorec.buf+ikbdiorec.head);
  
  /* restore interrupts */
  set_sr(old_sr);
  return value;
}

static void push_ikbdiorec(LONG value)
{
  ikbdiorec.tail += 4;
  if(ikbdiorec.tail >= ikbdiorec.size) {
    ikbdiorec.tail = 0;
  }
  if(ikbdiorec.tail == ikbdiorec.size) {
    /* iorec full */
    return;
  }
  *(LONG *)(ikbdiorec.buf+ikbdiorec.tail) = value;
}

/*=== interrupt routine support ===================================*/

/*
 * kbd_int : called by the interrupt routine for key events.
 */

VOID kbd_int(WORD scancode)
{
  LONG value = 0;      /* the value to push into iorec */
    
    
#if DBG_KBD
  kprint ("================\n ");
  kprintf ("Key-scancode: 0x%02x\n", scancode & 0xff);
  
  kprintf ("Key-shift bits: 0x%02x\n", shifty);
#endif

  if (scancode & KEY_RELEASED) {
    scancode &= ~KEY_RELEASED;       /* get rid of release bits */
    switch (scancode) {
    case KEY_RSHIFT:
      shifty &= ~MODE_RSHIFT;        /* clear bit */
      break;
    case KEY_LSHIFT:
      shifty &= ~MODE_LSHIFT;        /* clear bit */
      break;
    case KEY_CTRL:
      shifty &= ~MODE_CTRL;          /* clear bit */
      break;
    case KEY_ALT:
      shifty &= ~MODE_ALT;           /* clear bit */
      break;
    }
    /* The TOS does not return when ALT is set, to emulate
     * mouse movement using alt keys. This feature is not 
     * currently supported by EmuTOS.
     */
#if 0
    if(! (shifty & KEY_ALT))
#endif
    return;
  }

  switch (scancode) {
    case KEY_RSHIFT:
      shifty |= MODE_RSHIFT;         /* set bit */
      return;
    case KEY_LSHIFT:
      shifty |= MODE_LSHIFT;         /* set bit */
      return;
    case KEY_CTRL:
      shifty |= MODE_CTRL;           /* set bit */
      return;
    case KEY_ALT:
      shifty |= MODE_ALT;            /* set bit */
      return;
    case KEY_CAPS:
      shifty ^= MODE_CAPS;           /* toggle bit */
      return;
  }
  
  if (shifty & (MODE_LSHIFT|MODE_RSHIFT)) {
    if (scancode >= 0x3B && scancode <= 0x44) {
      scancode += 0x19;
      goto push_value;
    }
    value = shft_key_map[scancode];
  } else if (shifty & MODE_CAPS) {
    value = caps_key_map[scancode];
  } else {
    value = norm_key_map[scancode];
  }
  
  if (shifty & MODE_CTRL) {
    /* More complicated in TOS, but is it really necessary ? */
    value &= 0x1F;
  }
            
  if (shifty & MODE_ALT) {
    /* TODO, alt key */
  }

push_value:
  value += ((LONG)scancode & 0xFF)<<16;
  if (conterm & 0x8) {
    value += ((LONG)shifty) << 24;
  }
#if DBG_KBD
  kprintf ("KBD iorec: Pushing value 0x%08lx\n", value);
#endif
  push_ikbdiorec(value);
}


/*=== ikbd acia stuff ==================================================*/

/* can we send a byte to the ikbd ? */
LONG bcostat4(VOID)
{
  if(ikbd_acia.ctrl & ACIA_TDRE) {
    return -1;  /* OK */
  } else {
    /* Data register not empty */
    return 0;   /* not OK */
  }
}

/* send a byte to the IKBD */
VOID bconout4(WORD dev, WORD c)
{
  while(! bcostat4())
    ;
  ikbd_acia.data = c;
}

/* cnt = number of bytes to send less one */
VOID ikbdws(WORD cnt, LONG ptr)
{
  UBYTE *p = (UBYTE *)ptr;
  while(cnt-- >= 0) {
    bconout4(0, *p++);
  }
}


#define send_ikbd_acia(c) bconout4(0,c)

/*
 *	FUNCTION:  This routine resets the keyboard,
 *	  configures the MFP so we can get interrupts
 */
 
VOID	kbd_init(VOID)
{

    cputs("[    ] IKBD ACIA initialized ...\r");

    /* initialize ikbd ACIA */
    ikbd_acia.ctrl =
        ACIA_RESET;     /* master reset */

    ikbd_acia.ctrl =
        ACIA_RIE|       /* enable interrupts */
        ACIA_RLTID|     /* RTS low, TxINT disabled */
        ACIA_DIV64|     /* clock/64 */
        ACIA_D8N1S;  /* 8 bit, 1 stop, no parity */

    /* initialize the IKBD */
#if 1
    send_ikbd_acia(0x80);   /* reset IKBD */
    send_ikbd_acia(0x01);   /* also... */

    send_ikbd_acia(0x12);  /* disable mouse */
    send_ikbd_acia(0x1A);  /* disable joystick */
#endif

    bioskeys();

    cstatus(SUCCESS);
}
