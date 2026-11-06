! The extent of a BLOCK local in a DO CONCURRENT that calls a function is
! computed on the host, before the launch, and the kernel uses that value
! wherever the extent appears: converted to another kind (an integer(8)
! extent), in an IF and in a nested BLOCK. `nfun` and `nfun8` compute in
! real(8), which a device copy of them would not do on every GPU: here
! they must return 2.
module gpu_metal_639_m
    implicit none
contains
    pure integer function nfun(n)
        integer, intent(in) :: n(:)
        nfun = ceiling(1.0d0 + 1.0d-9 * real(n(1), 8))
    end function

    pure integer(8) function nfun8(n)
        integer, intent(in) :: n(:)
        nfun8 = ceiling(1.0d0 + 1.0d-9 * real(n(1), 8), 8)
    end function

    subroutine learn(n, y1, y2, y3, y4)
        integer, intent(in) :: n(:)
        real, intent(out) :: y1(:), y2(:), y3(:), y4(:)
        integer :: pair
        do concurrent (pair = 1:2)
            block
                real :: a(nfun8(n))
                a = real(pair)
                y1(pair) = sum(a)
            end block
        end do
        do concurrent (pair = 1:2)
            block
                real :: a(nfun(n))
                a = real(pair)
                if (pair > 0) then
                    y2(pair) = sum(a)
                end if
            end block
        end do
        do concurrent (pair = 1:2)
            block
                real :: a(nfun(n))
                a = real(pair)
                block
                    y3(pair) = sum(a)
                end block
            end block
        end do
        do concurrent (pair = 1:2)
            block
                real :: a(nfun(n))
                y4(pair) = 0.0
                if (pair > 0) then
                    a = real(pair)
                    y4(pair) = a(size(a))
                end if
                y4(pair) = y4(pair) + real(size(a))
            end block
        end do
    end subroutine
end module

program gpu_metal_639
    use gpu_metal_639_m
    implicit none
    integer :: nodes(2)
    real :: y1(2), y2(2), y3(2), y4(2)
    nodes = [2, 3]
    call learn(nodes, y1, y2, y3, y4)
    print *, y1, y2, y3, y4
    if (any(abs(y1 - [2.0, 4.0]) > 1e-6)) error stop
    if (any(abs(y2 - [2.0, 4.0]) > 1e-6)) error stop
    if (any(abs(y3 - [2.0, 4.0]) > 1e-6)) error stop
    if (any(abs(y4 - [3.0, 4.0]) > 1e-6)) error stop
end program
