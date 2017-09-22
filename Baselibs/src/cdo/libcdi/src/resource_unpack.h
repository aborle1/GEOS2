#ifndef RESOURCE_UNPACK_H
#define RESOURCE_UNPACK_H

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

enum
{ GRID      = 1,
  ZAXIS     = 2,
  TAXIS     = 3,
  INSTITUTE = 4,
  MODEL     = 5,
  STREAM    = 6,
  VLIST     = 7,
  RESH_DELETE,
  START     = 55555555,
  END       = 99999999
};

int reshUnpackResources(char * unpackBuffer, int unpackBufferSize,
                        void *context);

#endif

/*
 * Local Variables:
 * c-file-style: "Java"
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * show-trailing-whitespace: t
 * require-trailing-newline: t
 * End:
 */
