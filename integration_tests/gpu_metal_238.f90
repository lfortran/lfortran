! A Metal device function is emitted twice, once with its variable-address
! arguments in the `thread` address space and once in `device`.  A fixed-size
! array result argument is always a `thread` local of the caller, but the
! assumed-shape input arrays it is called with are kernel buffers in `device`,
! so tying both to the same address space left no viable overload and the
! shader failed to compile.  Only arguments the caller can supply in either
! space vary now.
program gpu_metal_238
implicit none

real :: a(3,4), b(3,4)
real :: v(3)
integer :: j

v = [1.0, 2.0, 3.0]

! Assumed-shape input (a device buffer) with a fixed-size array result.
a = 0.0
do concurrent (j = 1:4)
    a(:,j) = scaled(v, real(j))
end do
do j = 1, 4
    if (any(a(:,j) /= [1.0, 2.0, 3.0] * j)) error stop "scaled"
end do

! The same through two levels of device function calls.
b = 0.0
do concurrent (j = 1:4)
    b(:,j) = twice(v)
end do
do j = 1, 4
    if (any(b(:,j) /= [4.0, 8.0, 12.0])) error stop "twice"
end do

print *, a(:,2)
print *, b(:,1)
print *, "ok"

contains

    pure function scaled(x, s) result(r)
        real, intent(in) :: x(:)
        real, intent(in) :: s
        real :: r(3)
        integer :: i
        do i = 1, 3
            r(i) = s * x(i)
        end do
    end function

    pure function twice(x) result(r)
        real, intent(in) :: x(:)
        real :: r(3)
        r = scaled(scaled(x, 2.0), 2.0)
    end function

end program
