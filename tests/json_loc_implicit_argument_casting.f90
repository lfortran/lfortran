! With --implicit-argument-casting, passing the integer array `y` to the real
! dummy `x` of the explicit interface of `a` casts it through a pointer array;
! the `dimension` of that pointer array must carry the location of the
! argument `y`.
subroutine a(x)
real :: x(*)
end subroutine

subroutine b(y)
integer :: y(*)
interface
    subroutine a(x)
    real :: x(*)
    end subroutine
end interface
call a(y)
end subroutine
