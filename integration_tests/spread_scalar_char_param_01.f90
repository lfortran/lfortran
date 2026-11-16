! spread() of a scalar character constant in initialization expressions
! (found while fixing #13597)
module spread_scalar_char_param_01_m
implicit none
character(3), parameter :: mp(3) = spread("xyz", 1, 3)
end module

program spread_scalar_char_param_01
use spread_scalar_char_param_01_m, only: mp
implicit none
character(3), parameter :: p(3) = spread("abc", 1, 3)
character(*), parameter :: c = "de"
character(2), parameter :: pc(4) = spread(c, 1, 4)
character(5), parameter :: pad(2) = spread("ab", 1, 2)
character(2), parameter :: tr(2) = spread("abcd", 1, 2)
integer, parameter :: n = size(spread("abc", 1, 3))
character(3) :: q(3) = spread("pqr", 1, 3)
character(3) :: r(3)
character(len=20) :: out(2)

r = p
if (size(p) /= 3) error stop
if (any(r /= "abc")) error stop
if (size(pc) /= 4) error stop
if (any(pc /= "de")) error stop
if (len(pad) /= 5) error stop
if (pad(1) /= "ab   " .or. pad(2) /= "ab   ") error stop
if (any(tr /= "ab")) error stop
if (n /= 3) error stop
if (any(q /= "pqr")) error stop
if (size(mp) /= 3) error stop
if (any(mp /= "xyz")) error stop

out = spread(trim("ab/  "), 1, 2) // ["one", "two"]
if (out(1) /= "ab/one") error stop
if (out(2) /= "ab/two") error stop
end program
