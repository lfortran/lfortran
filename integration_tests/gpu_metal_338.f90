module gpu_metal_338_mod
implicit none
integer, parameter :: n = 8
contains

    ! `p` is bound to every second element of `col`. Reading `p(k)` in a
    ! gpu kernel once walked `col` one element at a time and returned the
    ! right number of the wrong elements; the device steps through `col` by
    ! the stride of the section now.
    pure function strided_sum(col) result(s)
        real, intent(in) :: col(:)
        real :: s
        integer :: k
        associate (p => col(1:n:2))
            s = 0.0
            do k = 1, size(p)
                s = s + p(k)
            end do
        end associate
    end function

end module

program gpu_metal_338
use gpu_metal_338_mod
implicit none
integer, parameter :: nt = 2
real :: a(n, nt), r(nt)
integer :: i, j

do j = 1, nt
    do i = 1, n
        a(i, j) = real(i)
    end do
end do
r = 0.0

do concurrent (j = 1:nt)
    r(j) = strided_sum(a(:, j))
end do

! col(1:8:2) is [1, 3, 5, 7]; the first four elements sum to 10, which is
! what dropping the step used to give.
do j = 1, nt
    if (abs(r(j) - 16.0) > 1.0e-4) error stop "strided associate"
end do

print *, r
print *, "PASS"
end program
