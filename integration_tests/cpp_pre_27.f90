program cpp_pre_27
implicit none
#define TARGET_LINE 80
#define TARGET_FILE "macro-comment.f90"
#define ID(x) x
#define COMMENTED(n, f) n/* (n, f) */f
#line/**/40 "commented.f90"
if (__LINE__ /= 40) error stop 1
if (__FILE__ /= "commented.f90") error stop 2
#line/**/50/**/"literal/*not a comment*/.f90"/**/
if (__LINE__ /= 50) error stop 3
if (__FILE__ /= "literal/*not a comment*/.f90") error stop 4
#/**/line/**/60 "prefix.f90"/***/
if (__LINE__ /= 60) error stop 5
if (__FILE__ /= "prefix.f90") error stop 6
#line /* spanning
comment */70 "multiline.f90" /* trailing
comment */
if (__LINE__ /= 70) error stop 7
if (__FILE__ /= "multiline.f90") error stop 8
#line ID/**/(TARGET_LINE)/**/TARGET_FILE/**/
if (__LINE__ /= 80) error stop 9
if (__FILE__ /= "macro-comment.f90") error stop 10
#line COMMENTED(85, "replacement.f90")
if (__LINE__ /= 85) error stop 18
if (__FILE__ /= "replacement.f90") error stop 19
# 90 "marker-comment.f90" /* source location */
if (__LINE__ /= 90) error stop 11
if (__FILE__ /= "marker-comment.f90") error stop 12
#/**/100/**/"marker/*literal*/.f90"/**/3/**/4/**/
if (__LINE__ /= 100) error stop 13
if (__FILE__ /= "marker/*literal*/.f90") error stop 14
#line/**/110/**/
if (__LINE__ /= 110) error stop 15
if (__FILE__ /= "marker/*literal*/.f90") error stop 16
#line 120 "preserved.f90"
#if 0
#line/**/900 "ignored\"ID(.f90"
# 910 "ignored-marker.f90"
#endif
if (__LINE__ /= 124) error stop 20
if (__FILE__ /= "preserved.f90") error stop 21
if ("a" // "b" /= "ab") error stop 17
end program
