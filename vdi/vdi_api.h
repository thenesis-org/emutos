// This file contains the public interface declarations used by VDI users.
#ifndef vdi_api_h
#define vdi_api_h

#include "emutos.h" // TODO: EmuTOS header should not be used.
#include <stdbool.h>

//--------------------------------------------------------------------------------
// Geometry.
//--------------------------------------------------------------------------------
/* Used by the VDI and by the desktop when calling the VDI. */
typedef union {
    struct {
        WORD x, y;
    };
    struct {
        WORD xMin, xMax;
    };
    WORD v[2];
} Point;

typedef union {
    struct {
        WORD x1, y1;
        WORD x2, y2;
    };
    struct {
        WORD xMin, yMin, xMax, yMax;
    };
    Point p[2];
} Rect;

typedef Rect ClippingRect;
typedef Rect Line;

//--------------------------------------------------------------------------------
// Parameter passing.
//--------------------------------------------------------------------------------
/*
#define vdi_Contrl_opcode 0
#define vdi_Contrl_ptsin 1
#define vdi_Contrl_ptsout 2
#define vdi_Contrl_intin 3
#define vdi_Contrl_intout 4
#define vdi_Contrl_subOpcode 5
#define vdi_Contrl_handle 6
*/

// VDI contrl array.
typedef struct {
    WORD opcode;
    WORD inputVertexNb; // ptsin
    WORD outputVertexNb; // ptsout
    WORD inputIntNb; // intin
    WORD outputIntNb; // intout
    WORD subOpcode;
    WORD workstationHandle;
    WORD specific[5];
} vdi_Contrl;

typedef struct {
    vdi_Contrl *contrl;
    WORD *intin;
    WORD *ptsin;
    WORD *intout;
    WORD *ptsout;
} vdi_Parameters;

//--------------------------------------------------------------------------------
// Writing modes.
//--------------------------------------------------------------------------------
// Writing modes. Used by vswr_mode(), vrt_cpyfm(), vqf_attributes(), vql_attributes(), vqm_attributes(), vqt_attributes().
typedef enum vdi_WritingMode_ {
    vdi_WritingMode_min =         1,
    vdi_WritingMode_replace =     1,
    vdi_WritingMode_transparent = 2,
    vdi_WritingMode_xor =         3,
    vdi_WritingMode_erasing =     4,
    vdi_WritingMode_max =         4,
    vdi_WritingMode_default =     vdi_WritingMode_replace
} vdi_WritingMode;

//--------------------------------------------------------------------------------
// Filling.
//--------------------------------------------------------------------------------
// Used by vsf_interior().
typedef enum vdi_FillingStyle_ {
    vdi_FillingStyle_min     = 0,
    vdi_FillingStyle_hollow  = 0,
    vdi_FillingStyle_solid   = 1,
    vdi_FillingStyle_pattern = 2,
    vdi_FillingStyle_hatch   = 3,
    vdi_FillingStyle_user    = 4,
    vdi_FillingStyle_max     = 4,
    vdi_FillingStyle_default = vdi_FillingStyle_hollow
} vdi_FillingStyle;

// Hatching style. Used by vsf_style() when fill style is hatch.
#define vdi_HatchStyle_min 1
#define vdi_HatchStyle_max 12
#define vdi_HatchStyle_default  1

// Pattern style. Used by vsf_style() when fill style is pattern.
#define vdi_PatternStyle_min 1
#define vdi_PatternStyle_max 24
#define vdi_PatternStyle_default 1

// Line style. Used by vsl_type().
#define vdi_LineStyle_min 1 /* for  */
#define vdi_LineStyle_max 7
#define vdi_LineStyle_default 1

//--------------------------------------------------------------------------------
// Line ending.
//--------------------------------------------------------------------------------
// Line ending types. Used by vsl_ends().
typedef enum vdi_LineEndStyle_ {
    vdi_LineEndStyle_min     = 0,
    vdi_LineEndStyle_square  = 0,
    vdi_LineEndStyle_arrow   = 1,
    vdi_LineEndStyle_round   = 2,
    vdi_LineEndStyle_max     = 2,
    vdi_LineEndStyle_default = vdi_LineEndStyle_square
} vdi_LineEndStyle;

//--------------------------------------------------------------------------------
// Generalized device primitive.
//--------------------------------------------------------------------------------
typedef enum {
    vdi_GdpType_bar = 1,
    vdi_GdpType_arc = 2,
    vdi_GdpType_pieSlice = 3,
    vdi_GdpType_circle = 4,
    vdi_GdpType_ellipse = 5,
    vdi_GdpType_ellipticalArc = 6,
    vdi_GdpType_ellipticalPieSlice = 7,
    vdi_GdpType_roundedBox = 8,
    vdi_GdpType_filledRoundedBox = 9,
    vdi_GdpType_justifiedText = 10,
    vdi_GdpType_bezier = 13,
} vdi_GdpType;

//--------------------------------------------------------------------------------
// Marker style.
//--------------------------------------------------------------------------------
// Marker style. Used by vsm_type().
#define vdi_MarkerStyle_min     1
#define vdi_MarkerStyle_max     6
#define vdi_MarkerStyle_default 3

// Text style bits.
#define vdi_TextStyle_thickened 1
#define vdi_TextStyle_light 2
#define vdi_TextStyle_skewed 4
#define vdi_TextStyle_underscored 8
#define vdi_TextStyle_outlined 16
#define vdi_TextStyle_shadowed 32

//--------------------------------------------------------------------------------
// Mouse cursor.
//--------------------------------------------------------------------------------
typedef struct {
    WORD mf_xhot;
    WORD mf_yhot;
    WORD mf_nplanes;
    WORD mf_bg; // mask colour index
    WORD mf_fg; // data colour index
    UWORD mf_mask[16];
    UWORD mf_data[16];
} MFORM;

//--------------------------------------------------------------------------------
// Raster.
//--------------------------------------------------------------------------------
// Raster data informations (called MFDB). Used by VDI and Line-A.
typedef struct {
    void *fd_addr; // Address of the form data. If NULL, the rest of the structure is filled with the screen infos.
    WORD fd_w; // Form width in pixels.
    WORD fd_h; // Form height in pixels.
    WORD fd_wdwidth; // Form width in words.
    WORD fd_stand; // Format: 0 = device specific, 1 = VDI format.
    WORD fd_nplanes; // Number of planes.
    WORD fd_r1; // Reserved (set to 0).
    WORD fd_r2;
    WORD fd_r3;
} vdi_RasterData;

//--------------------------------------------------------------------------------
// Blitting.
//--------------------------------------------------------------------------------
// 76-byte Line-A BITBLT struct passing parameters to bitblt.
typedef struct {
    WORD b_wd;          /* +00 width of block in pixels */
    WORD b_ht;          /* +02 height of block in pixels */
    WORD plane_ct;      /* +04 number of consecutive planes to blt */
    UWORD fg_col;       /* +06 foreground color (logic op table index:hi bit) */
    UWORD bg_col;       /* +08 background color (logic op table index:lo bit) */
    UBYTE op_tab[4];    /* +10 logic ops for all fore and background combos */
    WORD s_xmin;        /* +14 minimum X: source */
    WORD s_ymin;        /* +16 minimum Y: source */
    UWORD * s_form;     /* +18 source form base address */
    WORD s_nxwd;        /* +22 offset to next word in line  (in bytes) */
    WORD s_nxln;        /* +24 offset to next line in plane (in bytes) */
    WORD s_nxpl;        /* +26 offset to next plane from start of current plane */
    WORD d_xmin;        /* +28 minimum X: destination */
    WORD d_ymin;        /* +30 minimum Y: destination */
    UWORD * d_form;     /* +32 destination form base address */
    WORD d_nxwd;        /* +36 offset to next word in line  (in bytes) */
    WORD d_nxln;        /* +38 offset to next line in plane (in bytes) */
    WORD d_nxpl;        /* +40 offset to next plane from start of current plane */
    UWORD * p_addr;     /* +42 address of pattern buffer   (0:no pattern) */
    WORD p_nxln;        /* +46 offset to next line in pattern  (in bytes) */
    WORD p_nxpl;        /* +48 offset to next plane in pattern (in bytes) */
    WORD p_mask;        /* +50 pattern index mask */

    /* these frame parameters are internally set */
    WORD p_indx;        /* +52 initial pattern index */
    UWORD * s_addr;     /* +54 initial source address */
    WORD s_xmax;        /* +58 maximum X: source */
    WORD s_ymax;        /* +60 maximum Y: source */
    UWORD * d_addr;     /* +62 initial destination address */
    WORD d_xmax;        /* +66 maximum X: destination */
    WORD d_ymax;        /* +68 maximum Y: destination */
    WORD inner_ct;      /* +70 blt inner loop initial count */
    WORD dst_wr;        /* +72 destination form wrap (in bytes) */
    WORD src_wr;        /* +74 source form wrap (in bytes) */
} vdi_BlitParameters;

//--------------------------------------------------------------------------------
// VDI functions codes.
//--------------------------------------------------------------------------------
#define vdi_Function_openWorkstation 1
#define vdi_Function_closeWorkstation 2
#define vdi_Function_clearWorkstation 3

#define vdi_Function_escape 5
#define vdi_Function_drawPolyline 6

#define vdi_Function_drawText 8

#define vdi_Function_setCharHeight 12

#define vdi_Function_setLineType 15
#define vdi_Function_setLineWidth 16
#define vdi_Function_setLineColor 17

#define vdi_Function_setTextColor 22
#define vdi_Function_setFillInterior 23
#define vdi_Function_setFillStyle 24
#define vdi_Function_setFillColor 25

#define vdi_Function_setLocatorInput 28

#define vdi_Function_getString 31
#define vdi_Function_setWritingMode 32

#define vdi_Function_setInputMode 33

#define vdi_Function_getTextAttributes 38

#define vdi_Function_inquireExtended 102

#define vdi_Function_copyOpaqueRaster 109
#define vdi_Function_transformForm 110
#define vdi_Function_setCurrentForm 111

#define vdi_Function_setCustomLineStyle 113
#define vdi_Function_fillRectangle 114

#define vdi_Function_setTimerVector 118

#define vdi_Function_copyTransparentRaster 121
#define vdi_Function_showCursor 122
#define vdi_Function_hideCursor 123
#define vdi_Function_queryMouseState 124
#define vdi_Function_setMouseButtonVector 125
#define vdi_Function_setMouseMoveVector 126
#define vdi_Function_setMouseDrawingVector 127
#define vdi_Function_getKeyboardState 128
#define vdi_Function_setClipRegion 129

#define vdi_Function_setMouseWheelVector 134

//--------------------------------------------------------------------------------
// TODO: VDI functions callable directly.
//--------------------------------------------------------------------------------

#endif
