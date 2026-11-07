program openmp_116
! OpenMP conditional compilation: a character context on a `!$` line ending
! with `&` continues on the next `!$` line after the sentinel, its blanks and
! an optional `&`
implicit none
character(20) :: s, t, u, v, w
integer :: n
s = "x"
!$ s = "ab&
!$&cd"
if (s /= "abcd") error stop

t = "x"
!$ t = 'e''&
!$   &f' // "g&
! a comment line between continued lines

!$ &h"
if (t /= "e'fgh") error stop

u = "x"
!$ u = "i&
!$ j"
if (u /= "ij") error stop

n = 0
!$ n = len_trim("abc&
!$&de")
if (n /= 5) error stop
v = "x"
!$ v = 'ab&
! don't let this quote end the literal
!$&cd'
if (v /= "abcd") error stop

w = "x"
!$ w = 'ab'&
!$&'cd'
if (w /= "ab'cd") error stop
!$ w = 'ab'&
!$& // 'cd'
if (w /= "abcd") error stop
print *, s, t, u, v, w, n
end program
