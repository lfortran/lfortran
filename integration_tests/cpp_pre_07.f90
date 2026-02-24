program cpp_pre_07
implicit none
integer :: x

! Test that #ifdef with extra tokens after the macro name
! is handled correctly (extra tokens are ignored).

#ifdef UNDEFINED_MACRO && OTHER
x = 1
#else
x = 2
#endif

if (x /= 2) error stop

#define DEFINED_MACRO

#ifdef DEFINED_MACRO && OTHER
x = 10
#else
x = 20
#endif

if (x /= 10) error stop

#ifndef UNDEFINED_MACRO && OTHER
x = 100
#else
x = 200
#endif

if (x /= 100) error stop

print *, "ok"
end program
