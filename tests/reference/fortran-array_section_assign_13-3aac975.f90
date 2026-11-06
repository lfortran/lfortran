module array_section_assign_13_mod
implicit none
type :: sgd_t
    integer(4) :: a = 1
    integer(4) :: start_index = 1
    real(4), dimension(:), allocatable :: velocity
end type sgd_t

contains

subroutine shift(self, m)
    real(4), dimension(:), pointer :: __libasr_created__array_section_
    real(4), dimension(:), pointer :: __libasr_created__array_section_pointer_
    real(4), dimension(:), allocatable :: __libasr_created_real_binop_left_
    integer(4), intent(in) :: m
    type(sgd_t), intent(inout) :: self
    deallocate(__libasr_created_real_binop_left_)
    allocate(__libasr_created_real_binop_left_((m - 1 - self%start_index)/1 + 1))
    __libasr_created__array_section_pointer_ => self%velocity(self%start_index:m - 1)
    __libasr_created_real_binop_left_ = __libasr_created__array_section_pointer_
    __libasr_created__array_section_ => self%velocity(self%a:m)
    __libasr_created__array_section_ = __libasr_created_real_binop_left_ + 1.00000000e+00
end subroutine shift

subroutine upd(self, g)
    real(4), dimension(:), pointer :: __libasr_created__array_section_
    real(4), dimension(:), pointer :: __libasr_created_real_binop_right_
    integer(4) :: end_index
    real(4), dimension(:), intent(in) :: g
    type(sgd_t), intent(inout) :: self
    end_index = self%start_index + size(g) - 1
    __libasr_created_real_binop_right_ => self%velocity(self%start_index:end_index)
    __libasr_created__array_section_ => self%velocity(self%start_index:end_index)
    __libasr_created__array_section_ = 5.00000000e-01*__libasr_created_real_binop_right_ - g
end subroutine upd

end module array_section_assign_13_mod

program array_section_assign_13
use array_section_assign_13_mod, only: sgd_t
use array_section_assign_13_mod, only: shift
use array_section_assign_13_mod, only: upd
implicit none
real(4), dimension(2), parameter :: __libasr_created__subroutine_call_upd = [1.00000000e+00, 1.00000000e+00]
type(sgd_t) :: s
allocate(s%velocity(4))
s%velocity = [2.00000000e+00, 4.00000000e+00, 6.00000000e+00, 8.00000000e+00]
s%start_index = 2
call upd(s, __libasr_created__subroutine_call_upd)
if (s%velocity(1) /= 2.00000000e+00 .or. s%velocity(2) /= 1.00000000e+00) then
    error stop
end if
if (s%velocity(3) /= 2.00000000e+00 .or. s%velocity(4) /= 8.00000000e+00) then
    error stop
end if
s%start_index = 1
s%a = 2
call shift(s, 4)
if (s%velocity(2) /= 3.00000000e+00 .or. s%velocity(3) /= 2.00000000e+00) then
    error stop
end if
if (s%velocity(4) /= 3.00000000e+00 .or. s%velocity(1) /= 2.00000000e+00) then
    error stop
end if
print *, s%velocity
end program array_section_assign_13
