#ifndef LFORTRAN_DEBUG_MAP_PATHS_H
#define LFORTRAN_DEBUG_MAP_PATHS_H

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

#define LFORTRAN_DEBUG_MAP_SUFFIX_LDD_TXT "_ldd.txt"
#define LFORTRAN_DEBUG_MAP_SUFFIX_LINES_TXT "_lines.txt"
#define LFORTRAN_DEBUG_MAP_SUFFIX_LINES_DAT "_lines.dat"
#define LFORTRAN_DEBUG_MAP_SUFFIX_LINES_DAT_TXT "_lines.dat.txt"
#define LFORTRAN_DEBUG_MAP_PATH_MAX 4096

int _lfortran_debug_map_path_from_exe(
    const char *exe_path, const char *suffix, char *out_path, size_t out_path_size);

#ifdef __cplusplus
}
#endif

#endif
