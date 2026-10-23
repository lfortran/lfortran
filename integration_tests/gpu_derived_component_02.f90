! A `do concurrent` in a procedure that defines a scalar component of a
! derived-type dummy argument in one iteration, and writes an allocatable
! array component of it in every iteration: the caller sees both.
module gpu_derived_component_02_m
    implicit none
    type :: u
        real, allocatable :: w(:)
        integer :: k
    end type
contains
    subroutine s(self)
        type(u), intent(inout) :: self
        integer :: i
        do concurrent (i = 1:3)
            if (i == 1) self%k = 7
        end do
    end subroutine
    subroutine t(self)
        type(u), intent(inout) :: self
        integer :: i
        do concurrent (i = 1:size(self%w))
            self%w(i) = self%w(i) + 1
            if (i == 2) self%k = 10 * i
        end do
    end subroutine
end module

program gpu_derived_component_02
    use gpu_derived_component_02_m
    implicit none
    type(u) :: y
    allocate(y%w(3))
    y%w = 1
    y%k = 5
    call s(y)
    print *, y%k
    if (y%k /= 7) error stop 1
    call t(y)
    print *, y%k, y%w
    if (y%k /= 20) error stop 2
    if (any(y%w /= 2)) error stop 3
end program
