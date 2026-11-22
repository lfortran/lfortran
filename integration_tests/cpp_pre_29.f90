program cpp_pre_29
implicit none
#define ID(x) x
#define LOC __LINE__
#line 40 "continued.f90"
#line \
__LINE__
if (__LINE__ /= 41) error stop 1
#line 50 "comment.f90"
#line /* spanning
comment */ __LINE__
if (__LINE__ /= 51) error stop 2
#line 60 "prefix.f90"
#/* spanning
comment */line __LINE__
if (__LINE__ /= 61) error stop 3
#line 70 "nested.f90"
#line ID(__LINE__\
)
if (__LINE__ /= 71) error stop 4
#line 80 "trailing.f90"
#line LOC\
 "trailing.f90"
if (__LINE__ /= 80) error stop 5
#line 90 "split.f90"
#line __LI\
NE__
if (__LINE__ /= 90) error stop 6
#line 100 "twice.f90"
#line \
\
__LINE__
if (__LINE__ /= 102) error stop 7
#line 1 "low.f90"
#line \
__LINE__
if (__LINE__ /= 2) error stop 8
end program
