! The extent of a BLOCK local in a DO CONCURRENT that calls a function is
! computed on the host, before the launch, and the kernel's loops over the
! local -- of an assignment to the whole array and of SUM -- run to that
! value. `nfun` computes in real(8), which a device copy of it would not do
! on every GPU: here it must return 2, so the kernel must not recompute it.
module gpu_metal_636_m
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
                a = real(pair)
                y(pair) = sum(a)
            end block
        end do
    end subroutine
end module

program gpu_metal_636
    use gpu_metal_636_m
    implicit none
    integer :: nodes(2)
    real :: y(2)
    nodes = [2, 3]
    call learn(nodes, y)
    print *, y
    if (abs(y(1) - 2.0) > 1e-6) error stop
    if (abs(y(2) - 4.0) > 1e-6) error stop
end program
