program class_143
! Test: allocate class(*) 2D array with source=reshape()
implicit none
class(*), allocatable :: x(:,:)

allocate(x, source=reshape([1, 2, 3, 4], shape=[2, 2]))

select type(x)
type is (integer)
    if (x(1,1) /= 1) error stop
    if (x(2,1) /= 2) error stop
    if (x(1,2) /= 3) error stop
    if (x(2,2) /= 4) error stop
class default
    error stop
end select

deallocate(x)

allocate(x, source=reshape([1.5, 2.5, 3.5, 4.5, 5.5, 6.5], shape=[2, 3]))

select type(x)
type is (real)
    if (size(x, 1) /= 2) error stop
    if (size(x, 2) /= 3) error stop
    if (abs(x(1,1) - 1.5) > 1.0e-6) error stop
    if (abs(x(2,1) - 2.5) > 1.0e-6) error stop
    if (abs(x(1,2) - 3.5) > 1.0e-6) error stop
class default
    error stop
end select

print *, "PASS"
end program
