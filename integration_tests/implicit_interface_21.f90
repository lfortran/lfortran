! Test sequence association: passing array element D(1,J) to assumed-size dummy
! This pattern is common in legacy Fortran (e.g., LAPACK) where a 2D array column
! is passed by referencing its first element: CALL SUB(A(1, N), M)
! The callee receives a pointer to A(1,N) and accesses subsequent elements.

subroutine print_sum(arr, n)
    implicit none
    real, intent(in) :: arr(*)
    integer, intent(in) :: n
    real :: s
    integer :: j
    s = 0.0
    do j = 1, n
        s = s + arr(j)
    end do
    if (abs(s - 50.0) > 1.0e-5) error stop
end subroutine

program implicit_interface_21
    implicit none
    real :: d(4, 3)
    integer :: i

    ! Initialize: column 1 = 1,2,3,4; column 2 = 11,12,13,14; column 3 = 21,22,23,24
    do i = 1, 4
        d(i, 1) = real(i)
        d(i, 2) = real(i + 10)
        d(i, 3) = real(i + 20)
    end do

    ! Pass D(1, 2) as start of column 2 to assumed-size dummy
    ! Sequence association: the subroutine sees arr(1)=11, arr(2)=12, arr(3)=13, arr(4)=14
    ! Sum should be 11+12+13+14 = 50
    call print_sum(d(1, 2), 4)
    print *, "PASSED"
end program
