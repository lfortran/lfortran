! The same section selected with scalar integer components on both sides,
! as in an optimizer update, is read in place, without a temporary, while a
! section selected with other components still needs one (issue #16105).
module array_section_assign_13_mod
    implicit none
    type :: sgd_t
        real, allocatable :: velocity(:)
        integer :: start_index = 1, a = 1
    end type
contains

    subroutine upd(self, g)
        class(sgd_t), intent(inout) :: self
        real, intent(in) :: g(:)
        integer :: end_index
        end_index = self%start_index + size(g) - 1
        self%velocity(self%start_index:end_index) = &
            0.5*self%velocity(self%start_index:end_index) - g
    end subroutine

    subroutine shift(self, m)
        class(sgd_t), intent(inout) :: self
        integer, intent(in) :: m
        self%velocity(self%a:m) = self%velocity(self%start_index:m-1) + 1.0
    end subroutine

end module

program array_section_assign_13
    use array_section_assign_13_mod
    implicit none
    type(sgd_t) :: s
    allocate(s%velocity(4))
    s%velocity = [2.0, 4.0, 6.0, 8.0]
    s%start_index = 2
    call upd(s, [1.0, 1.0])
    if (s%velocity(1) /= 2.0 .or. s%velocity(2) /= 1.0) error stop 1
    if (s%velocity(3) /= 2.0 .or. s%velocity(4) /= 8.0) error stop 2
    s%start_index = 1
    s%a = 2
    call shift(s, 4)
    if (s%velocity(2) /= 3.0 .or. s%velocity(3) /= 2.0) error stop 3
    if (s%velocity(4) /= 3.0 .or. s%velocity(1) /= 2.0) error stop 4
    print *, s%velocity
end program
