module json_loc_continuation
! The continuation in `a` and the comment lines are removed by the
! prescanner; the JSON `loc` of `b` must still point to lines 13-15.
contains
subroutine a(x)
real :: x
x = 1.0 + &
    2.0
end subroutine

! Comment

subroutine b(y)
real :: y
end subroutine
end module
