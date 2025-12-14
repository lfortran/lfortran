# Backup the original value of the requested library suffixes
set(_CMAKE_FIND_LIBRARY_SUFFIXES ${CMAKE_FIND_LIBRARY_SUFFIXES})

# Prefer static libraries first (.a on Unix, .lib on Windows). If not available,
# fall back to the default search (shared libraries) so builds work on systems
# that do not ship static zlib.
set(CMAKE_FIND_LIBRARY_SUFFIXES .a .lib)
find_package(ZLIB QUIET)

set(CMAKE_FIND_LIBRARY_SUFFIXES ${_CMAKE_FIND_LIBRARY_SUFFIXES})

if (NOT ZLIB_FOUND OR NOT ZLIB_LIBRARY)
    unset(ZLIB_FOUND)
    unset(ZLIB_LIBRARY CACHE)
    unset(ZLIB_LIBRARIES CACHE)
    find_package(ZLIB REQUIRED)
endif()

# Unset the temporary to not pollute the global namespace
unset(_CMAKE_FIND_LIBRARY_SUFFIXES)
