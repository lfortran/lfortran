! A `do concurrent` that defines an element of an array component of a
! derived-type variable in each iteration, which the host reads after the
! loop: of a local with an allocatable component, of a dummy argument, and
! of a polymorphic dummy argument in a type-bound procedure. Each element
! belongs to one iteration, so the variable is defined in place, not
! copied per thread.
module gpu_derived_component_09_m
    implicit none
    integer, parameter :: n = 100
    type :: grid_t
        real, allocatable :: w(:)
        real :: coef(n)
    end type
    type :: s_t
        real :: coef(n)
    contains
        procedure :: init
    end type
contains
    subroutine fill(g)
        type(grid_t), intent(inout) :: g
        integer :: i
        do concurrent (i = 1:n)
            g%w(i) = i
            g%coef(i) = 2 * i
        end do
    end subroutine

    subroutine init(self)
        class(s_t), intent(inout) :: self
        integer :: i
        do concurrent (i = 1:n)
            self%coef(i) = 3 * i
        end do
    end subroutine
end module

program gpu_derived_component_09
    use gpu_derived_component_09_m
    implicit none
    type(grid_t) :: g, h
    type(s_t) :: s
    integer :: i
    allocate(g%w(n), h%w(n))
    g%w = 1
    do concurrent (i = 1:n)
        g%coef(i) = 2 * i + g%w(i)
    end do
    print *, g%coef(7)
    if (g%coef(7) /= 15) error stop 1
    call fill(h)
    print *, h%coef(7), h%w(7)
    if (h%coef(7) /= 14) error stop 2
    if (h%w(7) /= 7) error stop 3
    call s%init()
    print *, s%coef(7)
    if (s%coef(7) /= 21) error stop 4
end program
