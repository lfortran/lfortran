program cpp_pre_28
implicit none
#define ID(x) x
#define LOCATION(n, f) n f
#define INDIRECT(n, f) LOCATION(n, f)
#define NAME "quoted\"ID(,)/*text*/.f90"
#define LITERAL(n) n "quoted\"ID(.f90"
#line ID(40 "quoted\"name.f90")
if (__LINE__ /= 40) error stop 1
#line 50 "quoted\"ID(.f90"
if (__LINE__ /= 50) error stop 2
#line LOCATION(60, "quoted\"ID(,),/*literal*/.f90")
if (__LINE__ /= 60) error stop 3
#line INDIRECT(70, NAME)
if (__LINE__ /= 70) error stop 4
#line 80 "trailing\\"
if (__LINE__ /= 80) error stop 5
if (__FILE__ /= 'trailing\\') error stop 6
#line ID(90 "multiple\\\"quotes\".f90")
if (__LINE__ /= 90) error stop 7
#line __LINE__ __FILE__
if (__LINE__ /= 91) error stop 8
#line LITERAL(100)
if (__LINE__ /= 100) error stop 9
if (ID('it''s') /= "it's") error stop 10
if (ID('path\\') /= 'path\\') error stop 11
end program
