module gpu_inline_loop_subroutine_01_m
    implicit none
contains
    ! Quantizes the four values of chunk c of x: their largest magnitude is
    ! the scale, the codes are packed in one word.
    pure subroutine quantize_chunk(x, xw, sx, c)
        real, intent(in) :: x(:)
        integer, intent(inout) :: xw(:)
        real, intent(inout) :: sx(:)
        integer, intent(in) :: c
        real :: a0, a1, a2, a3, amax, inv
        a0 = x(4*c-3)
        a1 = x(4*c-2)
        a2 = x(4*c-1)
        a3 = x(4*c)
        amax = max(max(abs(a0), abs(a1)), max(abs(a2), abs(a3)))
        sx(c) = amax/127.0
        inv = 0.0
        if (amax > 0.0) inv = 127.0/amax
        xw(c) = ior(ior(iand(int(a0*inv), 255), ishft(iand(int(a1*inv), 255), 8)), &
            ior(ishft(iand(int(a2*inv), 255), 16), ishft(iand(int(a3*inv), 255), 24)))
    end subroutine

    ! A scalar that is only assigned.
    pure subroutine scaled(v, s, r)
        real, intent(in) :: v, s
        real, intent(out) :: r
        r = v*s + 1.0
    end subroutine

    subroutine producer(x, y, xw, sx, z)
        real, contiguous, intent(in) :: x(:)
        real, contiguous, intent(out) :: y(:), sx(:), z(:)
        integer, contiguous, intent(out) :: xw(:)
        integer :: g, i, j
        do concurrent (g = 1:size(x)/64)
            block
                do concurrent (i = (g-1)*64+1:g*64)
                    block
                        y(i) = x(i)*2.0
                    end block
                end do
                do concurrent (j = (g-1)*16+1:g*16)
                    block
                        real :: r
                        call quantize_chunk(x, xw, sx, j)
                        call scaled(x(4*j), 2.0, r)
                        z(j) = r
                    end block
                end do
            end block
        end do
    end subroutine
end module

program gpu_inline_loop_subroutine_01
    use gpu_inline_loop_subroutine_01_m
    implicit none
    integer, parameter :: n = 256
    real :: x(n), y(n), sx(n/4), z(n/4), sx_ref(n/4), z_ref(n/4)
    integer :: xw(n/4), xw_ref(n/4), k, j, a(4), c
    real :: amax, inv
    ! The scale of every chunk is 1, so that the codes are exact.
    do k = 1, n
        x(k) = real(mod(k*37, 201) - 100)
        if (mod(k, 4) == 0) x(k) = 127.0
    end do
    call producer(x, y, xw, sx, z)
    do j = 1, n/4
        amax = maxval(abs(x(4*j-3:4*j)))
        sx_ref(j) = amax/127.0
        inv = 0.0
        if (amax > 0.0) inv = 127.0/amax
        do c = 1, 4
            a(c) = iand(int(x(4*j-4+c)*inv), 255)
        end do
        xw_ref(j) = ior(ior(a(1), ishft(a(2), 8)), ior(ishft(a(3), 16), ishft(a(4), 24)))
        z_ref(j) = x(4*j)*2.0 + 1.0
    end do
    if (any(y /= 2.0*x)) error stop 1
    if (any(abs(sx - sx_ref) > 1e-6)) error stop 2
    if (any(xw /= xw_ref)) error stop 3
    if (any(abs(z - z_ref) > 1e-6)) error stop 4
    print *, "ok"
end program
