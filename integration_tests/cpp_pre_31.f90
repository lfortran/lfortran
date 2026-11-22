program cpp_pre_31
implicit none
#define IGNORE(x) 1
#define ID(x) x
#define CURRENT_FILE __FILE__
#define FORWARD(x) IGNORE(x)
#define PAIR(a, b) IGNORE(a) + IGNORE(b)
#define PREFIX(x) ""x
#line 40 "quoted\"ID(.f90"
if (IGNORE(__FILE__) /= 1) error stop 1
if (IGNORE(ID(CURRENT_FILE)) /= 1) error stop 2
if (FORWARD(CURRENT_FILE) /= 1) error stop 3
if (PAIR(__FILE__, CURRENT_FILE) /= 2) error stop 4
if (FORWARD((ID(CURRENT_FILE))) /= 1) error stop 5
#line 60 "slashes\\\"ID(,),/*literal*/.f90"
if (IGNORE(ID(CURRENT_FILE)) /= 1) error stop 6
if (FORWARD(CURRENT_FILE) /= 1) error stop 7
#line 70 "control\n\t\"ID(.f90"
if (PAIR(CURRENT_FILE, CURRENT_FILE) /= 2) error stop 8
#line 80 "adjacent\"ID(.f90"
if (IGNORE(PREFIX(CURRENT_FILE)) /= 1) error stop 9
if (IGNORE(ID(""CURRENT_FILE)) /= 1) error stop 10
#line 90 "plain.f90"
if (ID(CURRENT_FILE) /= "plain.f90") error stop 11
if (ID('it''s') /= "it's") error stop 12
if (ID('path\\') /= 'path\\') error stop 13
end program
