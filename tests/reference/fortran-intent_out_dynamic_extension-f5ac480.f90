module intent_out_dynamic_base
implicit none
type :: base
    integer(4) :: inherited = 4
end type base

contains

subroutine forward(x)
    class(base), dimension(:, :), intent(inout), optional :: x
    call reset_array(x)
end subroutine forward

subroutine reset(x)
    class(base), intent(out) :: x
end subroutine reset

subroutine reset_any(x)
    class(*), intent(out) :: x
end subroutine reset_any

subroutine reset_array(x)
    class(base), dimension(-1:, 0:), intent(out), optional :: x
end subroutine reset_array

end module intent_out_dynamic_base

module intent_out_dynamic_extension
use intent_out_dynamic_base, only: base
implicit none
type :: part
    integer(4), dimension(:), allocatable :: data
    integer(4) :: n = 5
end type part
type, extends(base) :: extension
    integer(4) :: added = 9
    integer(4), dimension(:), allocatable :: data
    character(len=2, kind=1), dimension(2) :: labels = ["ab", "cd"]
    type(part), dimension(2) :: parts
    type(resource), allocatable :: payload
    character(len=1, kind=1), pointer :: pointer_char => null()
    character(len=4, kind=1) :: text = "abcd"
end type extension
type, extends(extension) :: leaf
    real(4) :: more = 3.00000000e+00
end type leaf
type :: resource
    integer(4) :: value = 5
contains
    final :: finish
end type resource
integer(4), save :: final_calls = 0

contains

subroutine finish(x)
    type(resource), intent(inout) :: x
    final_calls = final_calls + 1
end subroutine finish

end module intent_out_dynamic_extension

