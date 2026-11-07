program mangle_underscore_07
! Structure constructor of a local type in a procedure copied by
! pass_array_by_data (#16091)
implicit none
call show([1.0, 2.0])
print *, "ok"
contains
subroutine show(x)
    real, intent(in) :: x(:)
    type v
        integer :: k = 5
    end type
    type(v) :: d
    d = v(int(x(1)) + 10)
    if (d%k /= 11) error stop
end subroutine
end program
