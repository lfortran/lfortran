module intent_out_dynamic_extension
    use intent_out_dynamic_base, only: base
    implicit none
    integer :: final_calls = 0
    type :: resource
        integer :: value = 5
    contains
        final :: finish
    end type
    type :: part
        integer :: n = 5
        integer, allocatable :: data(:)
    end type
    type, extends(base) :: extension
        integer :: added = 9
        integer, allocatable :: data(:)
        type(resource), allocatable :: payload
        type(part) :: parts(2)
        character(len=4) :: text = 'abcd'
        character(len=2) :: labels(2) = ['ab', 'cd']
        character(len=1), pointer :: pointer_char => null()
    end type
    type, extends(extension) :: leaf
        real :: more = 3.0
    end type
contains
    subroutine finish(x)
        type(resource), intent(inout) :: x
        final_calls = final_calls + 1
    end subroutine
end module
