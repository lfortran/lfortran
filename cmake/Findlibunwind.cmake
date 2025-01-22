find_path(LIBUNWIND_INCLUDE_DIR libunwind.h)
find_library(LIBUNWIND_LIBRARY unwind)

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(LIBUNWIND DEFAULT_MSG LIBUNWIND_INCLUDE_DIR LIBUNWIND_LIBRARY)

add_library(p::libunwind INTERFACE IMPORTED)
set_property(TARGET p::libunwind PROPERTY INTERFACE_INCLUDE_DIRECTORIES
    ${LIBUNWIND_INCLUDE_DIR})
set_property(TARGET p::libunwind PROPERTY INTERFACE_LINK_LIBRARIES
    ${LIBUNWIND_LIBRARY})