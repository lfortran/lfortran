program line_continuation_07
! A numeric literal split across a CRLF continuation must stay one token.
implicit none
integer, parameter :: wp = kind(1.0d0)
real(wp), parameter :: a = 0.6612093864662645&
    &137_wp
if (abs(a - 0.6612093864662645137_wp) > 1.0e-18_wp) error stop
end program
