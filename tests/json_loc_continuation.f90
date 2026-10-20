module json_loc_continuation
! The prescanner joins the continuation in `a`, so later offsets no longer
! match the file; the JSON `loc` of `b` must still point to lines 13-15.
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
