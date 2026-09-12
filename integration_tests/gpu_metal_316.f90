! An allocatable component of an array of a derived type of a rank above
! one is decomposed into the flat device buffers by the column-major
! position of the element, lower bounds and all, so the kernel reads and
! writes the same element the host does.
program gpu_metal_316
implicit none
type :: t
    real, allocatable :: v(:,:)
end type
type(t), allocatable :: p(:,:,:)
real :: r(2)
integer :: i, j, k, a, b, n

allocate(p(0:1, 2:4, -1:0))
n = 0
do k = -1, 0
    do j = 2, 4
        do i = 0, 1
            n = n + 1
            allocate(p(i, j, k)%v(n, n + 1))
            do b = 1, n + 1
                do a = 1, n
                    p(i, j, k)%v(a, b) = real(1000 * n + 10 * a + b)
                end do
            end do
        end do
    end do
end do

! p(i,3,0) sits at column-major position i + 2*1 + 6*1, so its n is i + 9.
do concurrent (i = 0:1)
    r(i + 1) = p(i, 3, 0)%v(2, 3)
end do

do i = 0, 1
    if (abs(r(i + 1) - real(1000 * (i + 9) + 23)) > 1.0e-3) error stop
end do

do concurrent (i = 0:1)
    p(i, 3, 0)%v(1, 2) = real(7000 + i)
end do

do i = 0, 1
    if (abs(p(i, 3, 0)%v(1, 2) - real(7000 + i)) > 1.0e-3) error stop
end do

print *, "PASS"
end program
