! A parallel do nested in a parallel do whose inner loop calls a function
! contained in the program and an external function through an implicit
! interface.
program implicit_interface_98
    implicit none
    integer :: i, j, a(4,4), b(4,4)
    real, external :: g
    !$omp parallel do private(j)
    do i = 1, 4
        !$omp parallel do
        do j = 1, 4
            a(i,j) = sq(i) + int(g(real(j)))
            b(i,j) = int(g(real(i))) + j
        end do
        !$omp end parallel do
    end do
    !$omp end parallel do
    if (sum(a) /= 160) error stop 1
    if (sum(b) /= 80) error stop 2
    print *, sum(a), sum(b)
contains
    integer function sq(k)
        integer, intent(in) :: k
        sq = k*k
    end function
end program

real function g(y)
    real :: y
    g = y
end function
