! DO CONCURRENT and FORALL inside device code: the device code generators
! write them as nested C `for` loops that run the iterations in order.
module gpu_do_loop_02_m
    implicit none
contains

    pure subroutine fill(a, n)
        integer, intent(in) :: n
        real, intent(inout) :: a(n)
        integer :: k
        do concurrent (k = 1:n)
            a(k) = real(k)
        end do
        forall (k = n:1:-2) a(k) = -a(k)
    end subroutine

end module

program gpu_do_loop_02
    use gpu_do_loop_02_m, only: fill
    implicit none
    integer, parameter :: n = 8
    real :: x(4, 3, n), expected
    integer :: i, j, k

    do concurrent (i = 1:n)
        block
            real :: w(4)
            integer :: p, q
            call fill(w, 4)
            do concurrent (p = 1:4, q = 1:3)
                x(p, q, i) = w(p)*q + i
            end do
        end block
    end do

    do i = 1, n
        do k = 1, 3
            do j = 1, 4
                expected = j*k + i
                if (mod(j, 2) == 0) expected = -j*k + i
                if (abs(x(j, k, i) - expected) > 1e-6) error stop
            end do
        end do
    end do
    print *, "ok"
end program
