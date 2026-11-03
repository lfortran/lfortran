! A BLOCK local in a DO CONCURRENT whose extent calls a function, assigned
! a reversed view of itself. The assignment goes through a temporary in a
! BLOCK of its own, and both the temporary and the copy back over the
! local use the extent the host computes before the launch. `nfun`
! computes in real(8), which a device copy of it would not do on every
! GPU: here it must return 2.
module gpu_metal_638_m
    implicit none
contains
    pure integer function nfun(n)
        integer, intent(in) :: n(:)
        nfun = ceiling(1.0d0 + 1.0d-9 * real(n(1), 8))
    end function

    subroutine learn(n, y)
        integer, intent(in) :: n(:)
        real, intent(out) :: y(:)
        integer :: pair
        do concurrent (pair = 1:2)
            block
                real :: a(nfun(n))
                a = 1.0
                a(size(a)) = 2.0
                a = a(size(a):1:-1)
                y(pair) = a(1) * real(pair) + sum(a)
            end block
        end do
    end subroutine
end module

program gpu_metal_638
    use gpu_metal_638_m
    implicit none
    integer :: nodes(2)
    real :: y(2)
    nodes = [2, 3]
    call learn(nodes, y)
    print *, y
    if (abs(y(1) - 5.0) > 1e-6) error stop
    if (abs(y(2) - 7.0) > 1e-6) error stop
end program
