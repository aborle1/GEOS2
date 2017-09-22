#if defined(HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <stdarg.h>
#include <errno.h>
#include <cdi.h>
#include "cdo.h"
#include "cdo_int.h"
#include "process.h"
#include "error.h"

void pstreamCloseAll(void);

void cdiOpenError(int cdiErrno, const char *fmt, const char *path)
{	
  printf("\n");
  set_text_color(stderr, RESET, RED);
   fprintf(stderr, "%s: ", processInqPrompt());
  reset_text_color(stderr);
  set_text_color(stderr, RESET, BLACK);
  fprintf(stderr, fmt, path);
  reset_text_color(stderr);
   fprintf(stderr, "\n");

  fprintf(stderr, "%s\n", cdiStringError(cdiErrno));

  if ( cdiErrno == CDI_ELIBNAVAIL )
    {
      int byteorder;
      int filetype = cdiGetFiletype(path, &byteorder);

      switch (filetype)
	{
	case FILETYPE_GRB:
          break;
	case FILETYPE_GRB2:
          fprintf(stderr, "To create a CDO application with GRIB2 support use: ./configure --with-netcdf=<GRIB_API root directory> ...\n");
          break;
	case FILETYPE_SRV:
          break;
	case FILETYPE_EXT:
          break;
	case FILETYPE_IEG:
          break;
	case FILETYPE_NC:
	case FILETYPE_NC2:
	case FILETYPE_NC4:
	case FILETYPE_NC4C:
          {
            const char *ncv = (filetype == FILETYPE_NC4 || filetype == FILETYPE_NC4C) ? "4" : ((filetype == FILETYPE_NC2) ? "2" : "");
#if defined HAVE_LIBNETCDF
            fprintf(stderr, "CDO was build with a NetCDF version which doesn't support NetCDF%s data!\n", ncv);
#else
            fprintf(stderr, "To create a CDO application with NetCDF%s support use: ./configure --with-netcdf=<NetCDF%s root directory> ...\n", ncv, ncv);
#endif
            break;
          }
	default:
          break;
	}
    }
  
  if ( _ExitOnError ) pstreamCloseAll();
  if ( _ExitOnError ) exit(EXIT_FAILURE);
}

void cdoAbort(const char *fmt, ...)
{
  printf("\n");
  set_text_color(stderr, RESET, RED);
   fprintf(stderr, "%s (Abort): ", processInqPrompt());
  reset_text_color(stderr);
  
  set_text_color(stderr, RESET, BLACK);
  va_list args;	
  va_start(args, fmt);
  vfprintf(stderr, fmt, args);
  va_end(args);
  reset_text_color(stderr);
  fprintf(stderr, "\n");

  if ( _ExitOnError ) pstreamCloseAll();
  if ( _ExitOnError ) exit(EXIT_FAILURE);
}


void cdoWarning(const char *fmt, ...)
{
  if ( _Verbose )
    {
      set_text_color(stderr, BRIGHT, YELLOW);
      fprintf(stderr, "%s (Warning): ", processInqPrompt());
      reset_text_color(stderr);

      set_text_color(stderr, RESET, BLACK);
      va_list args;
      va_start(args, fmt);
      vfprintf(stderr, fmt, args);
      va_end(args);
      reset_text_color(stderr);
      fprintf(stderr, "\n");
    }
}


void cdoPrint(const char *fmt, ...)
{
  if ( ! cdoSilentMode )
    {
      set_text_color(stderr, RESET, GREEN);
      fprintf(stderr, "%s: ", processInqPrompt());
      reset_text_color(stderr);

      set_text_color(stderr, RESET, BLACK);
      va_list args;
      va_start(args, fmt);
      vfprintf(stderr, fmt, args);
      va_end(args);
      reset_text_color(stderr);
      fprintf(stderr, "\n");
    }
}


void cdoPrintBlue(const char *fmt, ...)
{
  if ( ! cdoSilentMode )
    {
      set_text_color(stderr, RESET, GREEN);
      fprintf(stderr, "%s: ", processInqPrompt());
      reset_text_color(stderr);

      set_text_color(stderr, RESET, BLUE);
      va_list args;
      va_start(args, fmt);
      vfprintf(stderr, fmt, args);
      va_end(args);
      reset_text_color(stderr);
      fprintf(stderr, "\n");
    }
}


void cdoPrintRed(const char *fmt, ...)
{
  if ( ! cdoSilentMode )
    {
      set_text_color(stderr, RESET, GREEN);
      fprintf(stderr, "%s: ", processInqPrompt());
      reset_text_color(stderr);

      set_text_color(stderr, RESET, RED);
      va_list args;
      va_start(args, fmt);
      vfprintf(stderr, fmt, args);
      va_end(args);
      reset_text_color(stderr);
      fprintf(stderr, "\n");
    }
}
