program array_bounds_check_27
    ! An unallocated actual passed through an implicit interface: the
    ! message names the procedure the user called, `sub`.
    implicit none
    real(8), allocatable :: a(:,:)
    real(8) :: b(2,2)
    external :: sub
    b = 1.0d0
    call sub(2, b)
    call sub(2, a)
end program

subroutine sub(n, x)
    implicit none
    integer, intent(in) :: n
    real(8) :: x(n, n)
    print *, n, x(1, 1)
end subroutine
