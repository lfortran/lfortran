module matmul_05_mod
    implicit none
contains
    pure function dense(n) result(B)
        integer, intent(in) :: n
        double precision, allocatable :: B(:,:)
        allocate(B(n,n))
        B = 1.0d0
    end function
end module

program matmul_05
    use matmul_05_mod, only: dense
    implicit none
    integer, parameter :: n = 5
    double precision :: x(n), y(n)
    integer :: i

    x = 1.0d0
    y = matmul(dense(n), x)

    do i = 1, n
        if (abs(y(i) - 5.0d0) > 1.0d-12) error stop
    end do
    print *, "PASS"
end program
