module json_loc_implicit_argument_casting_m
! With --implicit-argument-casting, passing the complex array `w` to the real
! dummy `a` casts it through a pointer array; the `dimension` of that pointer
! array must carry the location of the argument `w`.
implicit none
contains
subroutine realcomp(n, a)
integer, intent(in) :: n
real(8), intent(inout) :: a(n)
end subroutine

subroutine caller(w)
complex(8), intent(inout) :: w(5)
call realcomp(10, w)
end subroutine
end module
