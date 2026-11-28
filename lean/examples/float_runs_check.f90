! Driver for lean/examples/check_float_runs.py: reads `count`, then `count` lines
! `n bits` (bits = the binary64 pattern of x as a signed 64-bit integer) and
! prints, per line, the bit patterns of legendre_poly(n, x), kernel_sin(x)
! and kernel_fast(x). Then reads `hcount`, then `hcount` pairs of lines `n`
! and `xbits c1bits ... cnbits` and prints the bit pattern of p from
! horner(n, c, x, p).
program float_runs_check
use, intrinsic :: iso_fortran_env, only: dp => real64, i8 => int64
use legendre_mod, only: legendre_poly
use sin_kernels, only: kernel_sin, kernel_fast
use horner_mod, only: horner
implicit none
integer :: count, i, n
integer(i8) :: bits
integer(i8), allocatable :: cbits(:)
real(dp) :: x, p
real(dp), allocatable :: c(:)
read(*, *) count
do i = 1, count
    read(*, *) n, bits
    x = transfer(bits, x)
    print "(i0, 1x, i0, 1x, i0)", transfer(legendre_poly(n, x), bits), &
        transfer(kernel_sin(x), bits), transfer(kernel_fast(x), bits)
end do
read(*, *) count
do i = 1, count
    read(*, *) n
    if (allocated(cbits)) deallocate(cbits, c)
    allocate(cbits(n), c(n))
    read(*, *) bits, cbits
    x = transfer(bits, x)
    c = transfer(cbits, c)
    call horner(n, c, x, p)
    print "(i0)", transfer(p, bits)
end do
end program
