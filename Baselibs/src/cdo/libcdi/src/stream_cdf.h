#ifndef _STREAM_CDF_H
#define _STREAM_CDF_H

void   cdfDefVars(stream_t *streamptr);
void   cdfDefTimestep(stream_t *streamptr, int tsID);
int    cdfInqTimestep(stream_t *streamptr, int tsID);
int    cdfInqContents(stream_t *streamptr);
void   cdfDefHistory(stream_t *streamptr, int size, const char *history);
int    cdfInqHistorySize(stream_t *streamptr);
void   cdfInqHistoryString(stream_t *streamptr, char *history);

void   cdfEndDef(stream_t * streamptr);
void   cdfDefRecord(stream_t * streamptr);

void   cdfCopyRecord(stream_t *streamptr2, stream_t *streamptr1);

void   cdf_read_record(stream_t *streamptr, int memtype, void *data, int *nmiss);
void   cdf_write_record(stream_t *streamptr, int memtype, const void *data, int nmiss);

void   cdf_read_var(stream_t *streamptr, int varID, int memtype, void *data, int *nmiss);
void   cdf_write_var(stream_t *streamptr, int varID, int memtype, const void *data, int nmiss);

void   cdf_read_var_slice(stream_t *streamptr, int varID, int levelID, int memtype, void *data, int *nmiss);
void   cdf_write_var_slice(stream_t *streamptr, int varID, int levelID, int memtype, const void *data, int nmiss);

void   cdf_write_var_chunk(stream_t *streamptr, int varID, int memtype,
                           const int rect[][2], const void *data, int nmiss);

void cdfDefVarDeflate(int ncid, int ncvarid, int deflate_level);
void cdfDefTime(stream_t* streamptr);

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
