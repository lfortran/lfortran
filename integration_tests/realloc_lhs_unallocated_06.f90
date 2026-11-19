! OPEN, INQUIRE, REWIND, BACKSPACE, ENDFILE, FLUSH and CLOSE change only the
! variables they name: a local array assigned after them is still certainly
! unallocated, and is allocated without a test.
module realloc_lhs_unallocated_06_m
implicit none
contains
subroutine io_then_assign(n, s)
    integer, intent(in) :: n
    real, intent(out) :: s
    real, allocatable :: x(:)
    integer :: u, i, ios
    logical :: opened
    open(newunit=u, status="scratch", form="formatted", iostat=ios)
    if (ios /= 0) error stop 1
    inquire(unit=u, opened=opened)
    if (.not. opened) error stop 2
    write(u, *) n
    endfile(u)
    backspace(u)
    rewind(u)
    flush(u)
    read(u, *) i
    close(u)
    if (i /= n) error stop 3
    x = [(real(i), i = 1, n)]
    if (size(x) /= n) error stop 4
    s = sum(x)
end subroutine
end module

program realloc_lhs_unallocated_06
use realloc_lhs_unallocated_06_m
implicit none
real :: s
call io_then_assign(4, s)
if (abs(s - 10) > 1e-6) error stop 6
print *, "ok"
end program
