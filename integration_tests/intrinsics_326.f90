program intrinsics_326
integer :: A(3)
A = [1, 9, -1]
call temp(A)
contains
subroutine temp(xpt)
    integer, intent(in) :: xpt(:)
    logical :: y(size(xpt))
    integer :: iubd
    real :: subd_test(size(xpt))
    subd_test = [1.0, 9.0, -1.0]
    y = .true.
    print *,  minloc(subd_test, mask = y)
    if (any(minloc(subd_test, mask = y) /= 3)) error stop
end subroutine
end program
