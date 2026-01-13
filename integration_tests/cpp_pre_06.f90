program cpp_pre_06
implicit none

#define A 10
#define B 20    // trailing comment
#define C A + B /* block comment */

#define ADD(x,y) ((x) + (y))
#define MUL(x,y) ((x) * (y))   // comment after macro

#undef UNUSED_MACRO

#ifdef A            // should be true
#define FLAG1 1
#else
#define FLAG1 0
#endif

#ifndef UNDEF_MACRO /* block comment */
#define FLAG2 1
#endif

#if A > 5
#define FLAG3 1
#elif A < 0
#define FLAG3 -1
#else
#define FLAG3 0
#endif

#if defined(A) && !defined(NOT_DEFINED)
#define FLAG4 1
#else
#define FLAG4 0
#endif

#if FLAG1
  #if FLAG2
    #define FLAG5 1
  #else
    #define FLAG5 0
  #endif
#endif

integer :: x
x = ADD(A, B) + MUL(2, 3)

print *, "A =", A
print *, "B =", B
print *, "C =", C
print *, "ADD(A,B) =", ADD(A,B)
print *, "MUL(2,3) =", MUL(2,3)
print *, "FLAG1 =", FLAG1
print *, "FLAG2 =", FLAG2
print *, "FLAG3 =", FLAG3
print *, "FLAG4 =", FLAG4
print *, "FLAG5 =", FLAG5
print *, "x =", x

end program cpp_pre_06
