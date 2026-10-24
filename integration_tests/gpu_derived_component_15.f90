! A local derived-type variable, not read after a `do concurrent`, that
! every iteration uses as a workspace: an array component of it assigned
! whole and passed with it to a procedure that defines another component,
! and a large array component assigned whole. Each thread works on a copy
! of its own, which nothing puts back, or on a kernel local when the value
! is too large to copy.
module gpu_derived_component_15_m
    implicit none
    type :: work_t
        real :: t(3)
        real :: s
    end type
    type :: big_t
        real :: t(2000)
    end type
contains
    pure subroutine sub(w)
        type(work_t), intent(inout) :: w
        w%s = w%t(1) + w%t(2) + w%t(3)
    end subroutine
end module

program gpu_derived_component_15
    use gpu_derived_component_15_m
    implicit none
    integer, parameter :: n = 1000
    type(work_t) :: w
    type(big_t) :: v
    real :: x(n), b(n), c(n)
    integer :: i
    do i = 1, n
        x(i) = i
    end do
    do concurrent (i = 1:n)
        w%t = x(i)
        call sub(w)
        b(i) = w%s
    end do
    do concurrent (i = 1:n)
        v%t = x(i)
        c(i) = v%t(1) + v%t(2000)
    end do
    print *, b(7), c(7)
    do i = 1, n
        if (b(i) /= 3 * i) error stop 1
        if (c(i) /= 2 * i) error stop 2
    end do
end program
