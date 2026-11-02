program gpu_metal_620
    ! integer(1) and integer(2) locals of a BLOCK, integer(2) and integer(1)
    ! arrays and scalars, and min/max reductions into them, in loops
    ! offloaded to the GPU.
    use iso_fortran_env, only: int8, int16
    implicit none
    integer :: y(2), z(4), w(3)
    integer(int16) :: a(4), a2(4), mn, mx
    integer(int8) :: b(4), b2(4), bmn, bmx, bs
    integer :: i

    call linear(2, y)
    if (any(y /= 3)) error stop 1

    do concurrent (i = 1:4)
        block
            integer(int16) :: t
            t = int(1000*i, int16)
            z(i) = 2*int(t)
        end block
    end do
    if (any(z /= [2000, 4000, 6000, 8000])) error stop 2

    call narrow(3, w)
    if (any(w /= -6)) error stop 3

    do concurrent (i = 1:4)
        a(i) = int(-8000*i + 7, int16)
    end do
    if (any(a /= [-7993_int16, -15993_int16, -23993_int16, -31993_int16])) &
        error stop 4

    bs = -2_int8
    do concurrent (i = 1:4)
        b(i) = int(i, int8) * bs
    end do
    if (any(b /= [-2_int8, -4_int8, -6_int8, -8_int8])) error stop 5

    mn = 0_int16; mx = -32000_int16
    do concurrent (i = 1:4) reduce(min: mn) reduce(max: mx)
        mn = min(mn, a(i))
        mx = max(mx, a(i))
    end do
    if (mn /= -31993_int16 .or. mx /= -7993_int16) error stop 6

    bmn = 0_int8; bmx = -100_int8
    do concurrent (i = 1:4) reduce(min: bmn) reduce(max: bmx)
        bmn = min(bmn, b(i))
        bmx = max(bmx, b(i))
    end do
    if (bmn /= -8_int8 .or. bmx /= -2_int8) error stop 7

    ! Every element is above -1, so a MIN identity of the wrong width
    ! (huge(0) truncates to -1) would win.
    a2 = [7_int16, 300_int16, 12000_int16, 5_int16]
    b2 = [9_int8, 100_int8, 3_int8, 42_int8]
    mn = huge(mn); bmn = huge(bmn)
    do concurrent (i = 1:4) reduce(min: mn, bmn)
        mn = min(mn, a2(i))
        bmn = min(bmn, b2(i))
    end do
    if (mn /= 5_int16 .or. bmn /= 3_int8) error stop 8
    print *, y, z, w, a, b, mn, mx, bmn, bmx
contains
    pure subroutine linear(parts, y)
        integer, intent(in) :: parts
        integer, intent(out) :: y(:)
        integer :: n
        do concurrent (n = 1:size(y))
            block
                integer(int16) :: partial_values(parts)
                integer :: p
                do concurrent (p = 1:parts)
                    partial_values(p) = int(p, int16)
                end do
                y(n) = sum(int(partial_values))
            end block
        end do
    end subroutine

    pure subroutine narrow(parts, w)
        integer, intent(in) :: parts
        integer, intent(out) :: w(:)
        integer :: n
        do concurrent (n = 1:size(w))
            block
                integer(int8) :: partial_values(parts)
                integer :: p
                do concurrent (p = 1:parts)
                    partial_values(p) = int(-p, int8)
                end do
                w(n) = sum(int(partial_values))
            end block
        end do
    end subroutine
end program
