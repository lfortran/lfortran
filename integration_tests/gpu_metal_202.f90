module gpu_metal_202_m
! Device helpers whose array arguments do not all live in the same memory
! space at every call site. `pair` reads two arrays, and `scaled` writes its
! result into a third one that the caller supplies.
implicit none
contains
    pure function pair(x, y) result(r)
    real, intent(in) :: x(:), y(:)
    real :: r
    integer :: j
    r = 0.0
    do j = 1, size(x)
        r = r + x(j) * y(j)
    end do
    end function

    pure function scaled(x, f) result(r)
    real, intent(in) :: x(4)
    real, intent(in) :: f
    real :: r(4)
    integer :: j
    do j = 1, 4
        r(j) = x(j) * f
    end do
    end function
end module

program gpu_metal_202
use gpu_metal_202_m
implicit none
integer, parameter :: n = 4
real :: a(n), b(n)
integer :: i
do i = 1, n
    a(i) = real(i)
end do
b = 0.0

do concurrent (i = 1:n)
    block
        real :: w(n), s(n)
        integer :: k
        do k = 1, n
            w(k) = 2.0
        end do
        ! `a` is a kernel argument and `s` is the thread's own storage, so
        ! this call writes a thread array from a device one.
        s = scaled(a, 3.0)
        ! One helper, three signatures: device with thread, thread with
        ! thread, and device with device.
        b(i) = pair(a, w) + pair(s, w) + pair(a, a)
    end block
end do

! pair(a, w)  = 2*(1+2+3+4)      = 20
! pair(s, w)  = 2*3*(1+2+3+4)    = 60
! pair(a, a)  = 1+4+9+16         = 30
do i = 1, n
    if (abs(b(i) - 110.0) > 1.0e-4) error stop
end do
print *, "ok"
end program
