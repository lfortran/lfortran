module wrapped
#define WRAPPED=1
#include "preprocessor_define_equals_generic.F90"
end module wrapped

program preprocessor_define_equals
    use wrapped, only: value
    if (value /= 1) error stop
end program preprocessor_define_equals
