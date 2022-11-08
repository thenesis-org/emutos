#include "vdi_interface.h"

#if CONF_WITH_VDI_LINEA

// Linea variables need to be placed in a specific area in RAM, that's why they are in their own file.
// The linker script can thus put it at the right place.
Linea lineaVars;

#endif
