module gpu_byte_perm_01_m
use, intrinsic :: iso_fortran_env, only: int32
implicit none
contains
    ! A byte of w in the low byte of a constant whose low byte is zero, in
    ! every spelling gpu_dp4a.cpp writes as one __byte_perm on an NVIDIA GPU.
    pure subroutine bytes(w, r)
        integer(int32), intent(in) :: w(:)
        integer(int32), intent(out) :: r(:, :)
        integer :: i
        do concurrent (i = 1:size(w))
            r(1, i) = ior(iand(w(i), 255), 1258291200)
            r(2, i) = ior(iand(shiftr(w(i), 8), 255), 1258291200)
            r(3, i) = ior(1258291200, iand(shiftr(w(i), 16), 255))
            r(4, i) = ior(shiftr(w(i), 24), 1258291200)
            r(5, i) = ior(ibits(w(i), 8, 8), 1258291200)
            r(6, i) = ior(iand(255, shifta(w(i), 24)), 1258291200)
            ! A constant with its top bit set: the result is negative, and
            ! stays a signed integer in the arithmetic around it.
            r(7, i) = ior(iand(shiftr(w(i), 16), 255), -16777216)/256
            ! Not matched: the constant's low byte is not zero.
            r(8, i) = ior(iand(w(i), 255), 1258291201)
        end do
    end subroutine

    ! The bytes of w as reals, dotted with x: the dequantization of a q8
    ! matrix-vector product.
    pure subroutine dot(w, x, y)
        integer(int32), intent(in) :: w(:, :)
        real, intent(in) :: x(:)
        real, intent(out) :: y(:)
        integer :: n
        do concurrent (n = 1:size(w, 2))
            block
                real :: a
                integer :: c
                a = 0
                do c = 1, size(w, 1)
                    a = a + (transfer(ior(iand(w(c, n), 255), 1258291200), 0.0) - 8388608.0)*x(4*c-3) &
                        + (transfer(ior(iand(shiftr(w(c, n), 8), 255), 1258291200), 0.0) - 8388608.0)*x(4*c-2) &
                        + (transfer(ior(iand(shiftr(w(c, n), 16), 255), 1258291200), 0.0) - 8388608.0)*x(4*c-1) &
                        + (transfer(ior(shiftr(w(c, n), 24), 1258291200), 0.0) - 8388608.0)*x(4*c)
                end do
                y(n) = a
            end block
        end do
    end subroutine

end module

program gpu_byte_perm_01
! Every value is an integer, so the sums are exact in any order.
use, intrinsic :: iso_fortran_env, only: int32
use gpu_byte_perm_01_m, only: bytes, dot
implicit none
integer, parameter :: m = 64, k = 512, rows = 3
integer(int32) :: w(m), r(8, m), wq(k/4, rows), b(0:3)
real :: x(k), y(rows), ref
integer :: i, j, c
do i = 1, m
    w(i) = int(mod(int(i, 8)*2654435761_8, 4294967296_8) - 2147483648_8, int32)
end do
call bytes(w, r)
do i = 1, m
    do j = 0, 3
        b(j) = iand(shiftr(w(i), 8*j), 255)
    end do
    if (r(1, i) /= 1258291200 + b(0)) error stop 1
    if (r(2, i) /= 1258291200 + b(1)) error stop 2
    if (r(3, i) /= 1258291200 + b(2)) error stop 3
    if (r(4, i) /= 1258291200 + b(3)) error stop 4
    if (r(5, i) /= 1258291200 + b(1)) error stop 5
    if (r(6, i) /= 1258291200 + b(3)) error stop 6
    if (r(7, i) /= (b(2) - 16777216)/256) error stop 7
    if (r(8, i) /= ior(b(0), 1258291201)) error stop 8
end do
do j = 1, rows
    do i = 1, k/4
        wq(i, j) = int(mod(int(i + 7*j, 8)*2246822519_8, 4294967296_8) - 2147483648_8, int32)
    end do
end do
do i = 1, k
    x(i) = real(mod(i, 7) - 3)
end do
call dot(wq, x, y)
do j = 1, rows
    ref = 0
    do c = 1, k/4
        do i = 0, 3
            ref = ref + real(iand(shiftr(wq(c, j), 8*i), 255))*x(4*c-3+i)
        end do
    end do
    if (y(j) /= ref) error stop 9
end do
print *, "ok"
end program
