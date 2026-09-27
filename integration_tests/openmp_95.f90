! A BLOCK after an outlined OpenMP region refers to the variables the region
! uses, both in its statements and in its declarations.
module openmp_95_m
implicit none
contains
subroutine s(b, n)
    integer, intent(in) :: n
    integer, intent(inout) :: b(n)
    integer :: i, c(4)
    !$omp parallel do
    do i = 1, n
        b(i) = 3*i
        c(mod(i, 4) + 1) = 0
    end do
    !$omp end parallel do
    block
        integer :: t(size(b))
        t = b
        block
            if (sum(t) /= 3*n*(n + 1)/2) error stop "subroutine: sum"
            if (b(2) /= 6) error stop "subroutine: b(2)"
            if (any(c /= 0)) error stop "subroutine: c"
        end block
    end block
end subroutine
end module

program openmp_95
use openmp_95_m, only: s
implicit none
integer :: i, b(8), a(5)
call s(a, 5)
if (a(5) /= 15) error stop "a(5)"
!$omp parallel do
do i = 1, 8
    b(i) = 2*i
end do
!$omp end parallel do
block
    integer :: w(size(b))
    w = b
    block
        if (w(3) /= 6) error stop "w(3)"
        if (b(8) /= 16) error stop "b(8)"
    end block
end block
if (b(3) /= 6) error stop "b(3)"
do concurrent (i = 1:8)
    b(i) = 3*i
end do
block
    if (b(2) /= 6) error stop "do concurrent: b(2)"
end block
print *, "ok"
end program
