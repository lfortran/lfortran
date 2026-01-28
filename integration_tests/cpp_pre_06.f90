#ifdef __GFORTRAN__
# define STRINGIFY_START(X) "&
# define STRINGIFY_END(X) &X"
#else /* default stringification */
# define STRINGIFY_(X) #X
# define STRINGIFY_START(X) &
# define STRINGIFY_END(X) STRINGIFY_(X)
#endif

program test
implicit none
character (len=:), allocatable :: astring
astring = STRINGIFY_START(MYMACRO)
STRINGIFY_END(MYMACRO)
if (astring /= "MYMACRO") error stop "stringify failed"
end program test
