! A `do concurrent` that defines an element of an array component of a
! large derived-type dummy argument in each iteration: the variable is
! defined in place, and no thread takes a copy of the whole value.
module gpu_derived_component_10_m
    implicit none
    integer, parameter :: n = 50000
    type :: big_t
        real :: arr(n)
        integer :: k
    end type
contains
    subroutine fill(p)
        type(big_t), intent(inout) :: p
        integer :: i
        do concurrent (i = 1:n)
            p%arr(i) = 2 * i
        end do
    end subroutine
end module

program gpu_derived_component_10
    use gpu_derived_component_10_m
    implicit none
    type(big_t) :: p
    call fill(p)
    print *, p%arr(7), p%arr(n)
    if (p%arr(7) /= 14) error stop 1
    if (p%arr(n) /= 2 * n) error stop 2
end program
