program openmp_117
! OpenMP conditional compilation: a `!$` line is a continuation line of the
! statement or character context continued on the line before it, and a
! `!$` line can be continued by an ordinary line
implicit none
character(20) :: s, t, u
integer :: k
k = 1 + &
!$ 2
if (k /= 3) error stop

s = 'ab&
!$&cd'
if (s /= "abcd") error stop

t = "x"
!$ t = "ab&
   &cd"
if (t /= "abcd") error stop

u = "x"
!$ u = 'ab' // &
   'cd'
if (u /= "abcd") error stop
print *, k, s, t, u
end program
