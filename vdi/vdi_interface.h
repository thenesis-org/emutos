// This file contains the protected interface declarations used internally by the OS. It should not be used outside the OS.
#ifndef vdi_interface_h
#define vdi_interface_h

#include "biosdefs.h"
#include "fonthdr.h"
#include "vdi/vdi_api.h"

#define VDI_EXTENDED_PALETTE (CONF_WITH_VIDEL || CONF_WITH_TT_SHIFTER)

#define vdi_Bezier_enabled 0 // Switch on bezier capability - entirely untested.

//--------------------------------------------------------------------------------
// VDI globals.
//--------------------------------------------------------------------------------
/* End of the VDI BSS section.
 * This is referenced by the OSHEADER */
extern UBYTE _vdi_bss_end[]; /* defined in vdi/vdi_end.S */

/* The VDI is just a library. It has no initialization routine.
 * To make it available, the BIOS just needs to install the vdi_trap function
 * below as trap #2 handler. */
void vdi_trap(void);

typedef struct vdi_VirtualWorkstation_ vdi_VirtualWorkstation;

/*
 * mouse cursor save area
 * NOTE: the lineA version of this only has the first 64 ULONGs,
 * to handle a maximum of 4 video planes.  Writing into area[64]
 * and above when referencing the lineA version will overwrite
 * other lineA variables with unpredictable results.
 */
#define vdi_MouseCursorSaveAreaFlag_valid 0x01 /* save area is valid */
#define vdi_MouseCursorSaveAreaFlag_longUsed 0x02 /* saved data is in longword format */
typedef struct vdi_MouseCursorSaveArea_ {
    WORD len; /* height of saved form */
    UWORD *addr; /* screen address of saved form */
    UBYTE stat; /* save status */
    char reserved;
    ULONG area[]; /* handle up to 8 video planes (8 * 16 longs) */
} vdi_MouseCursorSaveArea;

void vdi_setScreenShift(void);

#if CONF_WITH_VDI_LINEA

//--------------------------------------------------------------------------------
// Line-A global variables.
//--------------------------------------------------------------------------------
// Mouse sprite structure. This is a 1-bit 16x16 image.
typedef struct {
    WORD xhot;
    WORD yhot;
    WORD planes;
    WORD bg_col;
    WORD fg_col;
    UWORD maskdata[32]; /* mask & data are interleaved */
} vdi_MouseSprite;

#define linea_sizeTableLength 12
typedef union  {
    WORD array[linea_sizeTableLength + 3];
    struct {
        WORD charWidthMin;
        WORD charHeightMin;
        WORD charWidthMax;
        WORD charHeightMax;
        WORD lineWidthMin;
        WORD reserved0;
        WORD lineWidthMax;
        WORD reserved1;
        WORD markerWidthMin;
        WORD markerHeightMin;
        WORD markerWidthMax;
        WORD markerHeightMax;       
    };
} linea_SizeTable;

#define linea_inquireTableSize 45
#define linea_deviceTableSize 45

typedef struct Linea_ {
    /* Font related VDI variables */
    ULONG startAngle; // -910 start angle
    const Fonthead* text_currentFont; // -906 most recently used font
    UWORD reserved0[23]; // -902

    vdi_MouseSprite mouse_data; // storage for mouse sprite

    /* Extended workstation information */
    WORD workstation_inquireTable[linea_inquireTableSize]; // -782 information returned from a _vq_extnd()

    /* Workstation information */
    WORD workstation_deviceTable[linea_deviceTableSize]; // -692 information returned from a _v_opnwk()

    /* Mouse data */
    union {
        UBYTE mouse_infos; // Block of mouse pointer data
        struct {
            WORD mouse_positionX; // -602 mouse X position
            WORD mouse_positionY; // -600 mouse Y position
            WORD mouse_hiddenCount; // -598 Number of levels the mouse is hidden
            WORD mouse_buttonState; // -596 mouse button state
        };
    };

    /* RGB values for colors 0-15 */
    /*
     * The following line-A variables contain the VDI color palette entries.
     * linea_Palette_palette16 contains the first 16 entries; linea.palette_palette240 contains entries
     * 16-255 (only applicable for 8-plane resolutions).  Note that the
     * location of linea.palette_palette240 is not documented by Atari, but is derived from
     * disassembly of TOS ROMs, and source code for MagiC's VDI.
     */
    WORD palette_palette16[16][3]; // -594 48 WORDs of RGB data (color registers)

    /* Workstation linea.PTSOUT information */
    linea_SizeTable workstation_sizeTable; // -498 size table

    WORD keyboard_pressedKey; // -468 pressed key, aciii + scancode
    UWORD keyboard_inputModeChoice; // -466 input mode choice
    vdi_VirtualWorkstation* workstation_current; // -464 pointer to currently opened virtual workstation
    const Fonthead* text_defaultFont; // -460 default font of open workstation
    /*
     * fontRing is an array of four pointers, each of which points to
     * a linked list of font headers.  usage is as follows:
     *  fontRing[0]    system fonts that are available in all resolutions;
     *                  this is currently just the 6x6 font
     *  fontRing[1]    resolution-dependent system fonts; currently
     *                  the 8x8 and 8x16 fonts
     *  fontRing[2]    fonts loaded by GDOS; initially an empty list
     *  fontRing[3]    always NULL, marking the end of the list of lists
     */
    const Fonthead* text_fontRing[4]; // -456 all available fonts
    WORD text_fontCount; // -440 number of font ids in text_fontRing

    WORD circle_currentLineWidth; // -438 Line width for current circle
    UWORD keyboard_locatorMode; // -436 input mode, locator
    UBYTE reserved1[80]; // some space (input mode???)
    WORD circle_lineNum; // -354 Number of lines making up wide line - not sure if right here

    UWORD input_stringMode; // -352 input mode, string
    WORD input_valuatorMode; // -350 input mode, valuator
    UBYTE mouse_currentStatus; // -348 current mouse status
    UBYTE reserved2;
    WORD cursor_disableCount; // -346 disable depth count. (>0 => disabled)
    WORD mouse_newX; // -344 new mouse x&y position
    WORD mouse_newY;
    UBYTE mouse_drawRequested; // -340 non-zero means draw mouse form on vblank
    UBYTE mouse_modifyingCount; // -339 non-zero while mouse cursor is being modified

    WORD cursor_savedX; // -338 save area for cursor cell coords.
    WORD cursor_savedY; // -336 save area for cursor cell coords.
    ULONG retsav; // -334 I'm not sure if this is right here

    struct {
        vdi_MouseCursorSaveArea mouse_cursorSaveArea;
        ULONG mouse_cursorSavePixels[4 * 16]; // 4 planes * 16 lines. Must be contiguous with mouse_cursorSaveArea.
    };
    
    /* Timer vectors */
    ETV_TIMER_T timer_handle; // -66  timer interrupt vector
    ETV_TIMER_T timer_chain; // -62  timer interrupt vector save

    void (*mouse_handleUserButton)(void); // -58  user button vector
    void (*mouse_handleUserCursor)(void); // -54  user cursor vector
    void (*mouse_handleUserMotion)(void); // -50  user motion vector

    /* VDI ESC variables */
    UWORD font_cellHeight; // -46  cell height (width is 8)
    UWORD font_cellColumnNbMinus1; // -44  columns on the screen minus 1
    UWORD font_cellRowNbMinus1; // -42  rows on the screen minus 1
    UWORD font_cellLineSize; // -40  length (in bytes) of a line of characters
    WORD color_background; // -38  current background color
    WORD color_foreground; // -36  current foreground color
    UBYTE *cursor_address; // -34  current cursor address
    WORD cursor_offset; // -30  offset from begin of screen
    UWORD cursor_currentX; // -28  current cursor column
    UWORD cursor_currentY; // -26  current cursor row
    UBYTE cursor_blinkRate; // -24  cursor blink rate
    UBYTE cursor_blinkTimer; // -23  cursor blink timer.
    const UWORD *font_address; // -22  pointer to current monospace font
    UWORD font_lastAsciiCode; // -18  ascii code of last cell in font
    UWORD font_firstAsciiCode; // -16  ascii code of first cell in font
    UWORD font_cellWrap; // -14  font cell wrap
    UWORD screen_width; // -12 horizontal resolution in pixels
    const UWORD *font_offsetTable; // -10 pointer to font offset table
    UBYTE console_cellSystemStatus; // -6 video cell system status (was in words)
    UBYTE reserved4;
    UWORD screen_height; // -4 vertical resolution in pixels
    UWORD screen_lineSize; // -2 byte per screen line

    // Normal line-a variables now follow
    union {
        UBYTE base; // This is the base line-a pointer
        struct {
            UWORD screen_planeNb; // +0 number of color planes.
            UWORD screen_lineSize2; // +2 number of bytes/line.

            union {
                vdi_Parameters parameters;
                struct {
                    WORD *CONTRL; // +4   ptr to the linea_CONTRL array.
                    WORD *INTIN; // +8   ptr to the linea_INTIN array.
                    WORD *PTSIN; // +12  ptr to the linea_PTSIN array.
                    WORD *INTOUT; // +16  ptr to the linea_INTOUT array.
                    WORD *PTSOUT; // +20  ptr to the linea_PTSOUT array.
                };
            };

            // The following 4 variables are accessed by the line-drawing routines
            // as an array (to allow post-increment addressing).  They must be contiguous!!
            WORD color_bit0; // colour bit value for plane 0
            WORD color_bit1; // colour bit value for plane 1
            WORD color_bit2; // colour bit value for plane 2
            WORD color_bit3; // colour bit value for plane 3

            WORD line_lastFlag; // 0 => not last line of polyline.
            WORD line_mask; // line style mask.
            WORD writingMode; // writing mode.

            Rect rect;

            UWORD *pattern_address; // fill pattern pointer
            UWORD pattern_mask; // fill pattern "mask" (line count)
            WORD pattern_multiPlaneFillFlag; // multi-plane fill flag. (0 => 1 plane)

            WORD clipping_enabled; // clipping flag.
            ClippingRect clipping_rect;

            WORD ddaX; // accumulator for x DDA
            UWORD ddaIncrement; // the fraction to be added to the DDA
            WORD text_scaleDirection; // 0 if scale down, 1 if enlarge.
            WORD font_monospaced; // True if current font monospaced.
            WORD text_sourceX;
            WORD text_sourceY; // upper left of character in font file
            WORD text_destinationX;
            WORD text_destinationY; // upper left of destination on screen
            UWORD text_characterSizeX;
            UWORD text_characterSizeY; // width and height of character
            const UWORD *font_data; // pointer to font data
            WORD font_widthInBytes; // width of font form (in bytes), offset,segment and form width of font
            WORD font_style; // Requested text special effects
            WORD font_liteMask; // special effects
            WORD font_skewMask; // special effects
            WORD font_weight; // special effects
            WORD font_skewRoff; // skew above and below baseline
            WORD font_skewLoff;
            WORD text_scale; // True if current font scaled
            WORD text_upVector; // Text baseline vector
            WORD text_colorForeground; // text foreground color
            WORD *text_scratchBuffer; // pointer to base of scratch buffer
            WORD text_largeBuffer; // Offset to large text buffer
            UWORD text_background; // text background color
            UWORD raster_transparent; // Flag for Copy-raster-form (<>0 = Transparent)
            WORD (*seedFilling_abortCallback)(void); // Address of Routine for testing break out of lineA vdi_fill function

            // The following line-a variables are NOT documented by Atari.  However,
            // the offsets seem to be stable in all TOS releases from 3.00 up, and
            // NVDI 5.03 expects that at least some of them are present.  The source
            // code for MVDI (the MagiC VDI written by the authors of NVDI) also
            // references them.
            // The names are partly an EmuTOS invention, and partly stolen from MVDI.
            #if VDI_EXTENDED_PALETTE
            UBYTE reserved5[52];
            WORD palette_palette240[240][3]; // [240][3] words for saving color palette
            UBYTE reserved6[8];
            // These 4 variables are referenced by NVDI 5.03. We do not use them (yet?).
            UWORD color_bit4;
            UWORD color_bit5;
            UWORD color_bit6;
            UWORD color_bit7;
            // This area is only used when linea_Screen_planeNb == 8.
            struct {
                vdi_MouseCursorSaveArea mouse_cursorSaveAreaExt;
                ULONG mouse_cursorSavePixelsExt[8 * 16]; // 8 planes * 16 lines. Must be contiguous with mouse_cursorSaveAreaExt.
            };
            #endif
        };
    };
} Linea;

extern Linea lineaVars;

void linea_init(void);

#endif

//--------------------------------------------------------------------------------
// VDI user functions internal prototypes.
//--------------------------------------------------------------------------------
#if 0

void vdi_v_opnwk(vdi_VirtualWorkstation *);            /* 1 */
void vdi_v_clswk(vdi_VirtualWorkstation *);            /* 2 */
void vdi_v_clrwk(vdi_VirtualWorkstation *);            /* 3 */
/* void v_updwk(vdi_VirtualWorkstation *); */          /* 4 - not implemented */
void vdi_v_escape(vdi_VirtualWorkstation *);           /* 5 */

/**
 * Draw a polyline/wideline.
 */
void vdi_v_pline(vdi_VirtualWorkstation *);            /* 6 */
void vdi_v_pmarker(vdi_VirtualWorkstation *);          /* 7 */
void vdi_v_gtext(vdi_VirtualWorkstation *);            /* 8 */
void vdi_v_fillarea(vdi_VirtualWorkstation *);         /* 9 */
/* void vdi_v_cellarray(vdi_VirtualWorkstation *); */  /* 10 - not implemented */

void vdi_v_gdp(vdi_VirtualWorkstation *);              /* 11 */
void vdi_vst_height(vdi_VirtualWorkstation *);         /* 12 */
void vdi_vst_rotation(vdi_VirtualWorkstation *);       /* 13 */
void vdi_vs_color(vdi_VirtualWorkstation *);           /* 14 */
/**
 * Set line type for line-drawing functions.
 */
void vdi_vsl_type(vdi_VirtualWorkstation *);           /* 15 */
/**
 * vdi_vsl_width - Set line width
 */
void vdi_vsl_width(vdi_VirtualWorkstation *);          /* 16 */
/**
 * Sets the color for line-drawing.
 */
void vdi_vsl_color(vdi_VirtualWorkstation *);          /* 17 */
void vdi_vsm_type(vdi_VirtualWorkstation *);           /* 18 */
void vdi_vsm_height(vdi_VirtualWorkstation *);         /* 19 */
void vdi_vsm_color(vdi_VirtualWorkstation *);          /* 20 */

void vdi_vst_font(vdi_VirtualWorkstation *);           /* 21 */
void vdi_vst_color(vdi_VirtualWorkstation *);          /* 22 */
/*
 * Set fill style.
 */
void vdi_vsf_interior(vdi_VirtualWorkstation *);       /* 23 */
void vdi_vsf_style(vdi_VirtualWorkstation *);          /* 24 */
void vdi_vsf_color(vdi_VirtualWorkstation *);          /* 25 */

void vdi_vq_color(vdi_VirtualWorkstation *vwk);        /* 26 */
/* void vdi_vq_cellarray(vdi_VirtualWorkstation *); */ /* 27 - not implemented */
void vdi_v_locator(vdi_VirtualWorkstation *);          /* 28 */

void vdi_v_choice(vdi_VirtualWorkstation *);           /* 30 */

void vdi_v_string(vdi_VirtualWorkstation *);           /* 31 */
/** Set writing mode. */
void vdi_vswr_mode(vdi_VirtualWorkstation *);          /* 32 */
void vdi_vsin_mode(vdi_VirtualWorkstation *);          /* 33 */

/**
 * Inquire current polyline attributes.
 */
void vdi_vql_attributes(vdi_VirtualWorkstation *);     /* 35 */

void vdi_vqm_attributes(vdi_VirtualWorkstation *);     /* 36 */
/**
 * Inquire current fill area attributes.
 */
void vdi_vqf_attributes(vdi_VirtualWorkstation *);     /* 37 */
void vdi_vqt_attributes(vdi_VirtualWorkstation *);     /* 38 */
void vdi_vst_alignment(vdi_VirtualWorkstation *);      /* 39 */


void vdi_v_opnvwk(vdi_VirtualWorkstation *);           /* 100 */

void vdi_v_clsvwk(vdi_VirtualWorkstation *);           /* 101 */
void vdi_vq_extnd(vdi_VirtualWorkstation *);           /* 102 */
void vdi_v_contourfill(vdi_VirtualWorkstation *);      /* 103 */
void vdi_vsf_perimeter(vdi_VirtualWorkstation *);      /* 104 */
void vdi_v_get_pixel(vdi_VirtualWorkstation *);        /* 105 */

void vdi_vst_effects(vdi_VirtualWorkstation *);        /* 106 */
void vdi_vst_point(vdi_VirtualWorkstation *);          /* 107 */
/**
 * Sets the style of end point for line starting and ending points.
 */
void vdi_vsl_ends(vdi_VirtualWorkstation *);           /* 108 */
/**
 * Copy raster opaque
 * This function copies a rectangular raster area from source form to
 * destination form using the logic operation specified by the application.
 */
void vdi_vro_cpyfm(vdi_VirtualWorkstation *);          /* 109 */
/**
 * Transform screen bitmaps.
 *
 * Convert device-independent bitmaps to device-dependent and vice versa
 *
 * The major difference between the two formats is that, in the device-
 * independent ("standard") form, the planes are consecutive, while on
 * the Atari screen they are interleaved.
 */
void vdi_vr_trnfm(vdi_VirtualWorkstation *);           /* 110 */
/**
 * Transforms user defined cursor to device specific format.
 * Get the new values for the x and y-coordinates of the mouse hot
 * spot and the new color indices for the mouse mask and data.
 * Inputs:
 *     intin[0] - x coordinate of hot spot
 *     intin[1] - y coordinate of hot spot
 *     intin[2] - reserved for future use. must be 1
 *     intin[3] - Mask color index
 *     intin[4] - Data color index
 *     intin[5-20]  - 16 words of cursor mask
 *     intin[21-36] - 16 words of cursor data
 * Outputs:        None
 */
void vdi_vsc_form(vdi_VirtualWorkstation *);           /* 111 */
/**
 * vdi_vsf_udpat - Set user-defined fill pattern
 */
void vdi_vsf_udpat(vdi_VirtualWorkstation *);          /* 112 */
/**
 * Set user-defined line style.
 */
void vdi_vsl_udsty(vdi_VirtualWorkstation *);          /* 113 */
/**
 * Draw filled rectangle.
 */
void vdi_vr_recfl(vdi_VirtualWorkstation *);           /* 114 */
void vdi_vqin_mode(vdi_VirtualWorkstation *);          /* 115 */

void vdi_vqt_extent(vdi_VirtualWorkstation *);         /* 116 */
void vdi_vqt_width(vdi_VirtualWorkstation *);          /* 117 */
void vdi_vex_timv(vdi_VirtualWorkstation *);           /* 118 */
void vdi_vst_load_fonts(vdi_VirtualWorkstation *);     /* 119 */
void vdi_vst_unload_fonts(vdi_VirtualWorkstation *);   /* 120 */

/**
 * Copy raster transparent.
 *
 * This function copies a monochrome raster area from source form to a
 * color area. A writing mode and color indices for both 0's and 1's
 * are specified in the linea_INTIN array.
 */
void vdi_vrt_cpyfm(vdi_VirtualWorkstation *);          /* 121 */
/**
 * Show cursor.
 */
void vdi_v_show_c(vdi_VirtualWorkstation *);           /* 122 */
/**
 * Hide cursor.
 */
void vdi_v_hide_c(vdi_VirtualWorkstation *);           /* 123 */
/**
 * Query mouse position and button status.
 */
void vdi_vq_mouse(vdi_VirtualWorkstation *);           /* 124 */
/**
 * This routine replaces the mouse button change vector with
 * the address of a user-supplied routine.  The previous value
 * is returned so that it also may be called when there is a
 * change in the mouse button status.
 * Inputs:
 *    contrl[7], contrl[8] - pointer to user routine
 * Outputs:
 *    contrl[9], contrl[10] - pointer to old routine
 */
void vdi_vex_butv(vdi_VirtualWorkstation *);           /* 125 */
/**
 * This routine replaces the mouse coordinate change vector with the address
 * of a user-supplied routine.  The previous value is returned so that it
 * also may be called when there is a change in the mouse coordinates.
 *
 *  Inputs:
 *     contrl[7], contrl[8] - pointer to user routine
 *
 *  Outputs:
 *     contrl[9], contrl[10] - pointer to old routine
 */
void vdi_vex_motv(vdi_VirtualWorkstation *);           /* 126 */
/**
 * This routine replaces the mouse draw vector with the
 * address of a user-supplied routine.  The previous value
 * is returned so that it also may be called when the mouse
 * is to be drawn.
 *
 * Inputs:
 *    contrl[7], contrl[8] - pointer to user routine
 *
 * Outputs:
 *    contrl[9], contrl[10] - pointer to old routine
 *
 */
void vdi_vex_curv(vdi_VirtualWorkstation *);           /* 127 */
void vdi_vq_key_s(vdi_VirtualWorkstation *);           /* 128 */
/** Set Clip Region */
void vdi_vs_clip(vdi_VirtualWorkstation *);            /* 129 */
void vdi_vqt_name(vdi_VirtualWorkstation *);           /* 130 */

void vdi_vqt_fontinfo(vdi_VirtualWorkstation *);       /* 131 */

#if CONF_WITH_EXTENDED_MOUSE
/**
 * A Milan VDI extension
 * This routine replaces the mouse wheel vector with the
 * address of a user-supplied routine.  The previous value
 * is returned so that it also may be called when the mouse
 * wheel is used.
 * Inputs:
 *    contrl[7], contrl[8] - pointer to user routine
 * Outputs:
 *    contrl[9], contrl[10] - pointer to old routine
 */
void vdi_vex_wheelv(vdi_VirtualWorkstation *);         /* 134 */
#endif

#if vdi_Bezier_enabled
/* not in original TOS */
void vdi_v_bez_qual(vdi_VirtualWorkstation *);
void vdi_v_bez_control(vdi_VirtualWorkstation *);
void vdi_v_bez(vdi_VirtualWorkstation *vwk, Point *points, int count);
void vdi_v_bez_fill(vdi_VirtualWorkstation *vwk, Point *points, int count);
#endif

#endif

#endif
