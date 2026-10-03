program gpu_metal_329
    implicit none
    integer :: i, n
    real :: result(2, 3)
    n = 3
    call fill(n, result)
    do i = 1, 3
        if (abs(result(1, i) - 3*i) > 1.e-6) error stop 1
        if (abs(result(2, i) - 8*i) > 1.e-6) error stop 2
    end do
contains
    subroutine fill(n, result)
    integer, intent(in) :: n
    real, intent(out) :: result(2, 3)
    integer :: i
    do concurrent (i = 1:3)
        block
            real :: work(n)
            work = real(i)
            result(1, i) = sum(work)
        end block
        block
            real :: work(n + 1)
            work = real(2*i)
            result(2, i) = sum(work)
        end block
    end do
    end subroutine
end program
