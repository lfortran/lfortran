! An internal procedure that only calls another one, which uses a host
! variable whose type depends on another host variable (the length nlen):
! the frame it passes on refers to that length too.
module nested_vars_32_m
implicit none
contains
subroutine outer_varlen(nlen, vec)
    integer, intent(in) :: nlen
    character(len=nlen), dimension(:), allocatable, intent(out) :: vec
    call call_fill()
contains
    subroutine call_fill()
        call fill(1, 3)
    end subroutine
    subroutine fill(i, n)
        integer, intent(in) :: i
        integer, intent(in) :: n
        if (.not. allocated(vec)) allocate(vec(n))
        vec(i) = 'hi'
    end subroutine
end subroutine
end module
program nested_vars_32
use nested_vars_32_m
character(len=5), allocatable :: w(:)
call outer_varlen(5, w)
if (size(w) /= 3 .or. w(1) /= 'hi') error stop
print *, w(1)
end program
