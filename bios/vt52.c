/*
 * vt52.c - vt52 like screen handling routines
 *
 *
 * Copyright (C) 2013-2021 The EmuTOS development team
 * Copyright (C) 2004 Martin Doering
 *
 * Authors:
 *  MAD     Martin Doering
 *
 * This file is distributed under the GPL, version 2 or at your
 * option any later version.  See doc/license.txt for details.
 */

#include "vdi/vdi_interface.h"
#include "font.h"
#include "tosvars.h"            /* for save_row */
#include "sound.h"              /* for bell() */
#include "string.h"
#include "conout.h"
#include "vt52.h"
#include "bios.h"

#if CONF_SERIAL_CONSOLE_ANSI
/* We disable cursor home commands because it is more convenient */
# define SERIAL_CONSOLE_HONOR_HOME 0
#endif

/* converts from escape sequence value to column or row number */
#define POSITION_BIAS   32

/*
 * internal prototypes
 */
static void nop(void);
static void cursor_up(void);
static void cursor_down_impl(void);
static void cursor_down(void);
static void cursor_left_impl(void);
static void cursor_left(void);
static void cursor_right(void);
static void clear_and_home(void);
static void cursor_home(void);
static void reverse_linefeed(void);
static void erase_to_eos(void);
static void erase_to_eol_impl(void);
static void erase_to_eol(void);
static void insert_line(void);
static void delete_line(void);

static void set_fg(void);
static void set_bg(void);
static void erase_from_home(void);
static void cursor_off(void);
static void cursor_on(void);
static void cursor_on_cnt(void);
static void save_cursor_pos(void);
static void restore_cursor_pos(void);
static void erase_line(void);
static void erase_from_bol_impl(void);
static void erase_from_bol(void);
static void reverse_video_on(void);
static void reverse_video_off(void);
static void line_wrap_on(void);
static void line_wrap_off(void);

static void do_bell(void);
static void do_backspace(void);
static void do_tab(void);
static void ascii_lf(void);
static void ascii_cr(void);

/* handlers for the console state machine */
static void esc_ch1(WORD);
static void get_row(WORD);
static void get_column(WORD);

void blink(void);


/* jumptable for ESC + uppercase character */
static void (* const am_tab[])(void) = {
    cursor_up,          /* Cursor Up */
    cursor_down,        /* Cursor Down */
    cursor_right,       /* Cursor Right */
    cursor_left,        /* Cursor Left */
    clear_and_home,     /* Clear and Home */
    nop,                /* <ESC> F not supported */
    nop,                /* <ESC> G not supported */
    cursor_home,        /* Home */
    reverse_linefeed,   /* Reverse Line Feed */
    erase_to_eos,       /* Erase to End of Screen */
    erase_to_eol,       /* Erase to End of Line */
    insert_line,        /* Insert Line */
    delete_line         /* Delete Line */
};


/* jumptable for ESC + lowercase character */
static void (* const bw_tab[])(void) = {
    set_fg,             /* Set foreground color (1 more char) */
    set_bg,             /* Set background color (1 more char) */
    erase_from_home,    /* Erase from beginning of page */
    cursor_on,          /* Cursor On */
    cursor_off,         /* Cursor Off */
    nop,                /* <ESC> g not supported */
    nop,                /* <ESC> h not supported */
    nop,                /* <ESC> i not supported */
    save_cursor_pos,    /* Save Cursor Position */
    restore_cursor_pos, /* Restore Cursor position */
    erase_line,         /* Erase line */
    nop,                /* <ESC> m not supported */
    nop,                /* <ESC> n not supported */
    erase_from_bol,     /* Erase from Beginning of Line */
    reverse_video_on,   /* Reverse Video On */
    reverse_video_off,  /* Reverse Video Off */
    nop,                /* <ESC> r not supported */
    nop,                /* <ESC> s not supported */
    nop,                /* <ESC> t not supported */
    nop,                /* <ESC> u not supported */
    line_wrap_on,       /* Wrap at End of Line */
    line_wrap_off       /* No Wrap at End of Line */
};


/* jumptable for ASCII control codes */
static void (* const cntl_tab[])(void) = {
    do_bell,            /* 7 = bell */
    do_backspace,       /* 8 = backspace */
    do_tab,             /* 9 = Horizontal tab */
    ascii_lf,           /* 10 = Line feed */
    ascii_lf,           /* 11 = Vertical tab (Treated as line feed) */
    ascii_lf,           /* 12 = Form Feed (Treated as line feed) */
    ascii_cr            /* 13 = Carriage Return */
};


/*
 * cputc - console output
 */
void cputc(WORD ch)
{
#if CONF_SERIAL_CONSOLE && !CONF_SERIAL_CONSOLE_ANSI
    /* When no translation needs to be performed, output the character
     * immediately and unconditionally to the serial port.
     * When ANSI translation is required, output will be done in the
     * appropriate subroutines.
     */
    bconout(1, ch);
#endif

    if (!con_state) {
        /* vt52_init() has not been called yet, ignore */
        return;
    }

    /* based on our state goto the correct stub routine */
    (*con_state)(LOBYTE(ch));
}


/*
 * normal_ascii - state is normal output
 */
static void normal_ascii(WORD ch)
{
    /* If the character is printable ascii, go print it */
    if ( ch >= ' ' ) {
#if CONF_SERIAL_CONSOLE_ANSI
        bconout(1, ch);
#endif
        ascii_out(ch);
    }

    /* We handle the following control characters as special: */

    /* 7 = bell */
    /* 8 = backspace */
    /* 9 = Horizontal tab */
    /* 10 = Line feed */
    /* 11 = Vertical tab (Treated as line feed) */
    /* 12 = Form Feed (Treated as line feed) */
    /* 13 = Carriage Return */
    /* 27 = Escape (Start Command) */

    /* If escape character alter next state */
    else if ( ch == 0x1b ) {
        /* handle the control characters */
        con_state = esc_ch1;    /* set constate to handle esc codes */
    }

    /* Other control characters */
    else if ( ch >= 7 && ch <= 13 ) {
#if CONF_SERIAL_CONSOLE_ANSI
        bconout(1, ch);
#endif
        (*cntl_tab[ch - 7])();
    }
    /* All others are thrown away */
}


static void nop(void)
{
    return;
}


/*
 * do_bell - Ring the bell (in sound.c)
 */
static void do_bell(void)
{
    if (conterm & 4) {
        bell();
    }
}


/*
 * do_backspace - Same as Cursor Left
 */
static void do_backspace(void)
{
    cursor_left_impl();
}


/*
 * do_tab - calculate the tabulator values
 */
static void do_tab(void)
{
    move_cursor((lineaVars.cursor_currentX & 0xfff8) + 8, lineaVars.cursor_currentY);
}


/*
 * esc_ch1 - state is: handle first character of an escape sequence
 */
static void esc_ch1(WORD ch)
{
    con_state = normal_ascii;           /* default state is normal ascii */

    if ( (ch >= 'A') && (ch <= 'M') ) {     /* handle the range A-M */
        (*am_tab[ch-'A'])();
    }
    else if ( (ch >= 'b') && (ch <= 'w') ) {/* handle b-w */
        (*bw_tab[ch-'b'])();
    }
    else if ( ch == 'Y' ) {                 /* direct cursor addressing, need more chars */
        con_state = get_row;
    }
}


/*
 * get_row - state is: calculate row from character
 */
static void get_row(WORD ch)
{
    save_row = ch - POSITION_BIAS;      /* Remove space bias */
    con_state = get_column;
}


/*
 * get_column - state is: calculate column from character
 */
static void get_column(WORD ch)
{
    int row, col;
    char ansi[20];

    MAYBE_UNUSED(ansi);

    col = ch - POSITION_BIAS;           /* Remove space bias */
    row = save_row;
#if CONF_SERIAL_CONSOLE_ANSI
    sprintf(ansi, "\033[%d;%dH", row + 1, col + 1);
    bconout_str(1, ansi);
#endif
    move_cursor(col,row);
    con_state = normal_ascii;           /* Next char is not special */
}


/*
 * get_fg_col - state is: get foreground color
 */
static void get_fg_col(WORD ch)
{
#if CONF_SERIAL_CONSOLE_ANSI
    char ansi[10];
    sprintf(ansi, "\033[%dm", 30 + (ch & 7));
    bconout_str(1, ansi);
#endif

    /* set the foreground color from the 4 low-order bits only */
    lineaVars.color_foreground = ch & 0x0f;
    con_state = normal_ascii;           /* Next char is not special */
}


/*
 * get_bg_col - state is: get background color
 */
static void get_bg_col(WORD ch)
{
#if CONF_SERIAL_CONSOLE_ANSI
    char ansi[10];
    sprintf(ansi, "\033[%dm", 40 + (ch & 7));
    bconout_str(1, ansi);
#endif

    /* set the foreground color from the 4 low-order bits only */
    lineaVars.color_background = ch & 0x0f;
    con_state = normal_ascii;           /* Next char is not special */
}


static void set_fg(void)
{
    con_state = get_fg_col;             /* Next char is the FG color */
}


static void set_bg(void)
{
    con_state = get_bg_col;             /* Next char is the BG color */
}


/*
 * clear_and_home - Clear Screen and Home Cursor
 */
static void clear_and_home(void)
{
#if CONF_SERIAL_CONSOLE_ANSI
# if SERIAL_CONSOLE_HONOR_HOME
    bconout_str(1, "\033[H\033[2J");
# else
    if ( lineaVars.cursor_currentX )
        bconout_str(1, "\r\n");
# endif
#endif

    cursor_off();                               /* hide cursor */
    move_cursor(0, 0);                          /* cursor home */
    blank_out (0, 0, lineaVars.font_cellColumnNbMinus1, lineaVars.font_cellRowNbMinus1);       /* clear screen */
    cursor_on_cnt();                            /* show cursor */
}


/*
 * cursor_up - Alpha Cursor Up
 */
static void cursor_up(void)
{
#if CONF_SERIAL_CONSOLE_ANSI
    bconout_str(1, "\033[A");
#endif

    if ( lineaVars.cursor_currentY )
        move_cursor(lineaVars.cursor_currentX, lineaVars.cursor_currentY - 1);
}


/*
 * cursor_down_impl - Used by Cursor Down and LF
 */
static void cursor_down_impl(void)
{
    if ( lineaVars.cursor_currentY != lineaVars.font_cellRowNbMinus1)
        move_cursor(lineaVars.cursor_currentX, lineaVars.cursor_currentY + 1);
}


/*
 * cursor_down - Alpha Cursor Down
 */
static void cursor_down(void)
{
#if CONF_SERIAL_CONSOLE_ANSI
    bconout_str(1, "\033[B");
#endif

    cursor_down_impl();
}


/*
 * cursor_right - Alpha Cursor Right
 */
static void cursor_right(void)
{
#if CONF_SERIAL_CONSOLE_ANSI
    bconout_str(1, "\033[C");
#endif

    if ( lineaVars.cursor_currentX != lineaVars.font_cellColumnNbMinus1)
        move_cursor(lineaVars.cursor_currentX + 1, lineaVars.cursor_currentY);
}


/*
 * cursor_left_impl - Used by Cursor Left and Backspace
 */
static void cursor_left_impl(void)
{
    if ( lineaVars.cursor_currentX )
        move_cursor(lineaVars.cursor_currentX - 1, lineaVars.cursor_currentY);
}


/*
 * cursor_left - Alpha Cursor Left
 */
static void cursor_left(void)
{
#if CONF_SERIAL_CONSOLE_ANSI
    bconout_str(1, "\033[D");
#endif

    cursor_left_impl();
}


/*
 * cursor_home - Home Alpha Cursor
 */
static void cursor_home(void)
{
#if CONF_SERIAL_CONSOLE_ANSI
# if SERIAL_CONSOLE_HONOR_HOME
    bconout_str(1, "\033[H");
# else
    if ( lineaVars.cursor_currentX )
        bconout_str(1, "\r\n");
# endif
#endif

    move_cursor(0, 0);
}


/*
 * erase_to_eos - Erase to End of Screen
 */
static void erase_to_eos(void)
{
#if CONF_SERIAL_CONSOLE_ANSI
    bconout_str(1, "\033[J");
#endif

    erase_to_eol_impl(); /* erase to end of line */

    /* last line? */
    if ( lineaVars.cursor_currentY == lineaVars.font_cellRowNbMinus1 )
        return;    /* yes, done */

    /* erase from upper left corner to lower right corner */
    blank_out (0, lineaVars.cursor_currentY + 1, lineaVars.font_cellColumnNbMinus1, lineaVars.font_cellRowNbMinus1);
}


/*
 * erase_to_eol_impl - Erase to End of Line (implementation)
 */
static void erase_to_eol_impl(void)
{
    BOOL wrap = lineaVars.console_cellSystemStatus & M_CEOL;      /* save line wrap status */
    WORD s_cur_x, s_cur_y;

    lineaVars.console_cellSystemStatus &= ~M_CEOL;    /* clear EOL handling bit (overwrite) */

    cursor_off();               /* hide cursor */
    /* save the x and y coords of cursor */
    s_cur_x = lineaVars.cursor_currentX;
    s_cur_y = lineaVars.cursor_currentY;

    /* is x = x maximum? */
    if ( lineaVars.cursor_currentX == lineaVars.font_cellColumnNbMinus1 )
        ascii_out(' ');         /* output a space, the cell is odd! */
    else {
        /* test, if x is even or odd */
        if ( IS_ODD(lineaVars.cursor_currentX) )
            ascii_out(' ');     /* first output a space */

        blank_out (lineaVars.cursor_currentX, lineaVars.cursor_currentY, lineaVars.font_cellColumnNbMinus1, lineaVars.cursor_currentY);
    }

    /* restore wrap flag, the result of EOL test */
    if ( wrap )
        lineaVars.console_cellSystemStatus |= M_CEOL;

    move_cursor(s_cur_x, s_cur_y); /* restore cursor position */
    cursor_on_cnt();            /* show cursor */
}


/*
 * erase_to_eol - Erase to End of Line
 */
static void erase_to_eol(void)
{
#if CONF_SERIAL_CONSOLE_ANSI
    bconout_str(1, "\033[K");
#endif

    erase_to_eol_impl();
}


/*
 * reverse_video_on - Reverse Video On
 */
static void reverse_video_on(void)
{
#if CONF_SERIAL_CONSOLE_ANSI
    bconout_str(1, "\033[7m");
#endif

    lineaVars.console_cellSystemStatus |= M_REVID;    /* set the reverse bit */
}


/*
 * reverse_video_off - Reverse Video Off
 */
static void reverse_video_off(void)
{
#if CONF_SERIAL_CONSOLE_ANSI
    bconout_str(1, "\033[27m");
#endif

    lineaVars.console_cellSystemStatus &= ~M_REVID;    /* clear the reverse bit */
}


/*
 * reverse_linefeed - Reverse Index
 */
static void reverse_linefeed(void)
{
    /* if not at top of screen */
    if ( lineaVars.cursor_currentY ) {
        move_cursor(lineaVars.cursor_currentX, lineaVars.cursor_currentY - 1);
    }
    else {
        int savex = lineaVars.cursor_currentX;           /* save current x position */
        insert_line();                  /* Insert a line */
        move_cursor(savex, 0);
    }
}


/*
 * insert_line - Insert Line
 */
static void insert_line(void)
{
#if CONF_SERIAL_CONSOLE_ANSI
    bconout_str(1, "\033[L");
#endif
    cursor_off();               /* hide cursor */
    scroll_down(lineaVars.cursor_currentY);      /* scroll down 1 line & blank current line */
    move_cursor(0, lineaVars.cursor_currentY);   /* move cursor to beginning of line */
    cursor_on_cnt();            /* show cursor */
}


/*
 * delete_line - Delete Line
 */
static void delete_line(void)
{
#if CONF_SERIAL_CONSOLE_ANSI
    bconout_str(1, "\033[M");
#endif
    cursor_off();               /* hide cursor */
    scroll_up(lineaVars.cursor_currentY);        /* scroll up 1 line & blank bottom line */
    move_cursor(0, lineaVars.cursor_currentY);   /* move cursor to beginning of line */
    cursor_on_cnt();            /* show cursor */
}


/*
 * erase_from_home - Erase from Beginning of Page to cursor
 */
static void erase_from_home(void)
{
#if CONF_SERIAL_CONSOLE_ANSI
    bconout_str(1, "\033[1J");
#endif

    erase_from_bol_impl(); /* erase from beginning of line */

    /* first line? */
    if ( !lineaVars.cursor_currentY )
        return;    /* yes, done */

    /* erase rest of screen */
    blank_out (0, 0, lineaVars.font_cellColumnNbMinus1, lineaVars.cursor_currentY - 1);        /* clear screen */
}


/*
 * do_cnt_esce - Enable Cursor
 */
static void do_cnt_esce(void)
{
    invert_cell(lineaVars.cursor_currentX, lineaVars.cursor_currentY);        /* complement cursor */
    lineaVars.console_cellSystemStatus |= M_CVIS;                     /* set visibility bit */

    /* see if flashing is enabled */
    if ( lineaVars.console_cellSystemStatus & M_CFLASH ) {
        lineaVars.console_cellSystemStatus |= M_CSTATE;                   /* set cursor on */

        /* do not flash the cursor when it moves */
        lineaVars.cursor_blinkTimer = lineaVars.cursor_blinkRate;                   /* reset the timer */
    }
}


/*
 * cursor_on - Enable Cursor forced
 */
static void cursor_on(void)
{
#if CONF_SERIAL_CONSOLE_ANSI
    /* Disabled because function used from internal VT52 implementation */
    /* bconout_str(1, "\033[?25h"); */
#endif

    /* if disable count is zero (cursor still shown) then return */
    if ( !lineaVars.cursor_disableCount )
        return;

    lineaVars.cursor_disableCount = 0;                      /* reset the disable counter */
    do_cnt_esce();
}


/*
 * cursor_on_cnt - Enable Cursor (counted depth)
 */
static void cursor_on_cnt(void)
{
    /* if disable count is zero (cursor still shown) then return */
    if ( !lineaVars.cursor_disableCount )
        return;

    lineaVars.cursor_disableCount--;                        /* decrement the disable counter */
    if (!lineaVars.cursor_disableCount)
        do_cnt_esce();                  /* if 0, do the enable */
}


/*
 * cursor_off - Disable Cursor
 */
static void cursor_off(void)
{
#if CONF_SERIAL_CONSOLE_ANSI
    /* Disabled because function used from internal VT52 implementation */
    /* bconout_str(1, "\033[?25l"); */
#endif

    lineaVars.cursor_disableCount++;                        /* increment the disable counter */

    /* test and clear the visible state bit */
    if (!(lineaVars.console_cellSystemStatus & M_CVIS) )
        return;                         /* if already invisible, just return */

    lineaVars.console_cellSystemStatus &= ~M_CVIS;                /* make invisible! */

    /* see, if flashing is disabled */
    if ( ! (lineaVars.console_cellSystemStatus & M_CFLASH) ) {
        invert_cell(lineaVars.cursor_currentX, lineaVars.cursor_currentY);
    }
    /* see, if cursor is on or off */
    else if ( lineaVars.console_cellSystemStatus & M_CSTATE ) {
        lineaVars.console_cellSystemStatus &= ~M_CSTATE;    /* cursor off? */
        invert_cell(lineaVars.cursor_currentX, lineaVars.cursor_currentY);
    }
}


/*
 * save_cursor_pos - Save Cursor Position
 */
static void save_cursor_pos(void)
{
#if CONF_SERIAL_CONSOLE_ANSI
    /* Disabled because function used from internal VT52 implementation */
    /* bconout_str(1, "\033[s"); */
#endif

    lineaVars.console_cellSystemStatus |= M_SVPOS;    /* set "position saved" status bit */

    /* save the x and y coords of cursor */
    lineaVars.cursor_savedX = lineaVars.cursor_currentX;
    lineaVars.cursor_savedY = lineaVars.cursor_currentY;
}


/*
 * restore_cursor_pos - Restore Cursor Position
 */
static void restore_cursor_pos(void)
{
#if CONF_SERIAL_CONSOLE_ANSI
    /* Disabled because function used from internal VT52 implementation */
    /* bconout_str(1, "\033[u"); */
#endif

    if ( lineaVars.console_cellSystemStatus & M_SVPOS )
        move_cursor(lineaVars.cursor_savedX, lineaVars.cursor_savedY);      /* move to saved position */
    else
        move_cursor(0, 0);      /* if position was not saved, home cursor */

    lineaVars.console_cellSystemStatus &= ~M_SVPOS;    /* clear "position saved" status bit */
}


/*
 * erase_line - Erase Entire Line
 *
 * upper left coords. (0,y), lower right coords. (max,y)
 */
static void erase_line(void)
{
#if CONF_SERIAL_CONSOLE_ANSI
    bconout_str(1, "\033[2K\033[1G");
#endif

    cursor_off();               /* hide cursor */
    blank_out (0, lineaVars.cursor_currentY, lineaVars.font_cellColumnNbMinus1, lineaVars.cursor_currentY);   /* blank whole line */
    move_cursor(0, lineaVars.cursor_currentY);   /* move cursor to beginning of line */
    cursor_on_cnt();            /* show cursor */
}


/*
 * erase_from_bol_impl - Erase from Beginning of Line (implementation)
 *
 * upper left coords. (0,y)
 * lower right coords. (x,y)
 */
static void erase_from_bol_impl(void)
{
    WORD s_cur_x, s_cur_y;

    cursor_off();               /* hide cursor */
    /* save the x and y coords of cursor */
    s_cur_x = lineaVars.cursor_currentX;
    s_cur_y = lineaVars.cursor_currentY;

    /*
     * because blank_out() requires the ending x position to be
     * odd, we need to handle the two possibilities separately
     */
    if ( !IS_ODD(s_cur_x) ) {
        ascii_out(' ');     /* first output a space */
        if (s_cur_x)
            blank_out(0, s_cur_y, s_cur_x-1, s_cur_y);
    }
    else
        blank_out(0, s_cur_y, s_cur_x, s_cur_y);

    move_cursor(s_cur_x, s_cur_y); /* restore cursor position */
    cursor_on_cnt();            /* show cursor */
}


/*
 * erase_from_bol - Erase from Beginning of Line
 *
 * upper left coords. (0,y)
 * lower right coords. (x,y)
 */
static void erase_from_bol(void)
{
#if CONF_SERIAL_CONSOLE_ANSI
    bconout_str(1, "\033[1K");
#endif

    erase_from_bol_impl();
}


/*
 * line_wrap_on() - Wrap at End of Line
 */
static void line_wrap_on(void)
{
#if CONF_SERIAL_CONSOLE_ANSI
    bconout_str(1, "\033[7h");
#endif
    lineaVars.console_cellSystemStatus |= M_CEOL;    /* set the eol handling bit */
}


/*
 * line_wrap_off - Discard at End of Line
 */
static void line_wrap_off(void)
{
#if CONF_SERIAL_CONSOLE_ANSI
    bconout_str(1, "\033[7l");
#endif
    lineaVars.console_cellSystemStatus &= ~M_CEOL;    /* clear the eol handling bit */
}


/*
 * ascii_cr - carriage return
 */
static void ascii_cr(void)
{
    /* beginning of current line */
    move_cursor(0, lineaVars.cursor_currentY);
}


/*
 * ascii_lf - line feed
 */
static void ascii_lf(void)
{
    /* at bottom of screen? */
    if ( lineaVars.cursor_currentY != lineaVars.font_cellRowNbMinus1 )
        cursor_down_impl();
    else {
        cursor_off();                   /* yes, hide cursor */
        scroll_up(0);                   /* scroll up 1 line */
        cursor_on_cnt();                /* show cursor */
    }
}


/*
 * blink - cursor blink interrupt routine
 *
 * This routine may trash registers, when called from assembler!
 */
void blink(void)
{
    /* test visibility/semaphore bit */
    if (!(lineaVars.console_cellSystemStatus & M_CVIS) )
        return;    /* if invisible or blocked, return */

    /* test flash bit */
    if (!(lineaVars.console_cellSystemStatus & M_CFLASH) )
        return;    /* if not flashing, return */

    /* decrement cursor flash timer */
    if ( --lineaVars.cursor_blinkTimer )
        return;    /* if <> 0, return */

    lineaVars.cursor_blinkTimer = lineaVars.cursor_blinkRate;       /* else reset timer */

    /* toggle cursor state */
    if ( lineaVars.console_cellSystemStatus & M_CSTATE )
        lineaVars.console_cellSystemStatus &= ~M_CSTATE;    /* clear bit (overwrite) */
    else
        lineaVars.console_cellSystemStatus |= M_CSTATE;    /* set bit (overwrite) */

    /* fetch x and y coords and complement cursor */
    invert_cell(lineaVars.cursor_currentX, lineaVars.cursor_currentY);
}


/*
 * cursconf - cursor configuration
 *
 * Arguments:
 *
 *   function =
 *   0 - switch off cursor
 *   1 - switch on cursor
 *   2 - blinking cursor
 *   3 - not blinking cursor
 *   4 - set cursor blink rate
 *   5 - get cursor blink rate
 *
 * Bits:
 *   M_CFLASH - cursor flash on
 *   M_CVIS   - cursor visibility on
 */
WORD cursconf(WORD function, WORD operand)
{
    switch (function) {
    case 0:
        cursor_off();                   /* set cursor non-visible */
        break;
    case 1:
        cursor_on();                    /* set cursor visible */
        break;
    case 2:
        lineaVars.console_cellSystemStatus &= ~M_CFLASH;          /* unset cursor flash bit */
        break;
    case 3:
        lineaVars.console_cellSystemStatus |= M_CFLASH;           /* set cursor flash bit */
        break;
    case 4:
        lineaVars.cursor_blinkRate = LOBYTE(operand);     /* set cursor flash interval */
        break;
    case 5:
        return(lineaVars.cursor_blinkRate);               /* set cursor flash interval */
    }
    return 0;
}


/*
 * vt52_init - initialize the conout state machine
 */
void vt52_init(void)
{
    /* set font-related lineA variables */
    font_set_default();

    /* Initial cursor settings */
    lineaVars.cursor_currentX = 0;                       /* cursor to column 0, row 0 */
    lineaVars.cursor_currentY = 0;
    lineaVars.cursor_offset = 0;                       /* line offset is 0 */
    lineaVars.cursor_address = v_bas_ad;                /* set cursor to start of screen */

    lineaVars.console_cellSystemStatus = M_CFLASH;                /* cursor invisible, flash, nowrap, normal video */
    cursconf(4, 30);                    /* 0.5 second blink rate (@ 60Hz vblank) */
    lineaVars.cursor_blinkTimer = lineaVars.cursor_blinkRate;               /* load initial value to blink timer */
    lineaVars.cursor_disableCount = 1;                      /* cursor disabled 1 level deep */

    /* set foreground color depending on color depth */
    switch (lineaVars.screen_planeNb) {
    case 1:
        lineaVars.color_foreground = 1;
        break;
    case 2:
        lineaVars.color_foreground = 3;
        break;
    default:
        lineaVars.color_foreground = 15;
    }
    lineaVars.color_background = 0;

    con_state = normal_ascii;           /* Init conout state machine */

    clear_and_home();
}
