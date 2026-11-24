#line 200 "logical/include.f90"
if (CURRENT_LINE /= 200) error stop 6
if (CURRENT_FILE /= "logical/include.f90") error stop 7
#include "nested.h"
if (CURRENT_LINE /= 203) error stop 8
if (CURRENT_FILE /= "logical/include.f90") error stop 9
#define INCLUDED_LINE CURRENT_LINE
#define INCLUDED_FILE CURRENT_FILE
