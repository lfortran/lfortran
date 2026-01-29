#ifdef __GFORTRAN__
# define STRINGIFY_START(X) "&
# define STRINGIFY_END(X) &X"
#else /* default stringification */
# define STRINGIFY_(X) #X
# define STRINGIFY_START(X) &
# define STRINGIFY_END(X) STRINGIFY_(X)
#endif

#define MYMACRO 0.12.0

program test
implicit none
character (len=:), allocatable :: astring

astring = STRINGIFY_START(MYTEXT)
STRINGIFY_END(MYTEXT)
if (astring /= "MYTEXT") error stop "stringify failed for text"

astring = STRINGIFY_START(MYMACRO)
STRINGIFY_END(MYMACRO)
if (astring /= "0.12.0") error stop "stringify failed for macro"
end program test
