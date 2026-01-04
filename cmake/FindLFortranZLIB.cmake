# FindLFortranZLIB.cmake
# Finds the ZLIB compression library for LFortran
#
# Respects the USE_DYNAMIC_ZLIB variable:
#   OFF (default): Search for static libraries first (.a, .lib)
#   ON: Search for dynamic libraries first (.so, .dylib, .dll), then static
#
# If USE_DYNAMIC_ZLIB is OFF but no static library is available, fall back to
# a default search so builds work on systems that do not ship static zlib.

# Backup the original value of the requested library suffixes
set(_CMAKE_FIND_LIBRARY_SUFFIXES ${CMAKE_FIND_LIBRARY_SUFFIXES})

if (USE_DYNAMIC_ZLIB)
    if(WIN32)
        set(CMAKE_FIND_LIBRARY_SUFFIXES .dll .lib .a)
    elseif(APPLE)
        set(CMAKE_FIND_LIBRARY_SUFFIXES .dylib .so .a .lib)
    else()
        set(CMAKE_FIND_LIBRARY_SUFFIXES .so .a .lib)
    endif()
else()
    # Prefer static libraries first
    set(CMAKE_FIND_LIBRARY_SUFFIXES .a .lib)
endif()

find_package(ZLIB QUIET)

# Reset the library suffixes to the original value
set(CMAKE_FIND_LIBRARY_SUFFIXES ${_CMAKE_FIND_LIBRARY_SUFFIXES})
# Unset the temporary to not pollute the global namespace
unset(_CMAKE_FIND_LIBRARY_SUFFIXES)

if (NOT ZLIB_FOUND OR NOT ZLIB_LIBRARY)
    unset(ZLIB_FOUND)
    unset(ZLIB_LIBRARY CACHE)
    unset(ZLIB_LIBRARIES CACHE)
    find_package(ZLIB REQUIRED)
endif()

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(LFortranZLIB DEFAULT_MSG ZLIB_LIBRARY
    ZLIB_INCLUDE_DIRS)
