#ifndef LFORTRAN_PARSER_SOURCE_OFFSET_H
#define LFORTRAN_PARSER_SOURCE_OFFSET_H

#include <cstdint>

#include <libasr/checked_cast.h>

namespace LCompilers::LFortran {

// Source positions are stored as uint32_t in Location and LocationManager,
// which limits a single source buffer to 4 GiB. Convert a byte offset into
// such a buffer, raising an internal compiler error if it is out of range.
template <typename T>
inline uint32_t source_offset(T offset)
{
    return checked_integer_cast<uint32_t>(offset);
}

} // namespace LCompilers::LFortran

#endif // LFORTRAN_PARSER_SOURCE_OFFSET_H
