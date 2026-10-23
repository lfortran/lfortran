! An array component of a derived-type variable that every iteration of a
! `do concurrent` defines and then reads, whole or by elements that do not
! depend on the index of the loop, is a temporary of the iteration
! (Fortran 2018, 11.1.7.5): each thread keeps a private copy of it, for a
! dummy argument and for a variable a loop around the `do concurrent`
! reads again. An element of a large array component that one iteration
! defines, and the elements each iteration owns, are defined in place.
module gpu_derived_component_13_m
    implicit none
    type :: work_t
        real :: t(3)
    end type
    type :: field_t
        real :: u(100000)
    end type
contains
    subroutine whole(w, x, b)
        type(work_t), intent(inout) :: w
        real, intent(in) :: x(:)
        real, intent(out) :: b(:)
        integer :: i
        do concurrent (i = 1:size(b))
            w%t = x(i)
            b(i) = w%t(1) + 2 * w%t(2)
        end do
    end subroutine

    subroutine elements(w, x, b)
        type(work_t), intent(inout) :: w
        real, intent(in) :: x(:)
        real, intent(out) :: b(:)
        integer :: i
        do concurrent (i = 1:size(b))
            w%t(1) = x(i)
            w%t(2) = 2 * x(i)
            b(i) = w%t(1) + w%t(2)
        end do
    end subroutine

    subroutine boundary(f)
        type(field_t), intent(inout) :: f
        integer :: i
        do concurrent (i = 1:size(f%u))
            if (i == 1) then
                f%u(1) = -1
            else
                f%u(i) = i
            end if
        end do
    end subroutine
end module

program gpu_derived_component_13
    use gpu_derived_component_13_m
    implicit none
    integer, parameter :: n = 100000
    type(work_t) :: w
    type(field_t) :: f
    real, allocatable :: x(:), b(:)
    real :: c(100)
    integer :: i, step
    allocate(x(n), b(n))
    do i = 1, n
        x(i) = i
    end do
    call whole(w, x, b)
    do i = 1, n
        if (b(i) /= 3 * i) error stop 1
    end do
    call elements(w, x, b)
    do i = 1, n
        if (b(i) /= 3 * i) error stop 2
    end do
    call boundary(f)
    if (f%u(1) /= -1 .or. f%u(7) /= 7 .or. f%u(n) /= n) error stop 3
    do step = 1, 2
        w%t = 0
        do concurrent (i = 1:100)
            w%t = i
            c(i) = w%t(1) + step
        end do
    end do
    if (c(7) /= 9) error stop 4
    print *, b(7), f%u(1), f%u(n), c(7)
end program
