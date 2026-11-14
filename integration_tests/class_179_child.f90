module class_179_child_m
    use class_179_base_m, only: base
    implicit none
    integer, target, save, private :: default_target = 17
    integer, target, save, private :: numbers(3) = [1, 2, 3]
    type :: payload
        integer :: number = 33
    end type
    type(payload), target, save, private :: box
    type :: inner
        integer :: mark = 42
        integer, allocatable :: values(:)
        integer, pointer :: ref => null()
        integer, pointer :: restored => default_target
    end type
    type, extends(base) :: child
        type(inner) :: part(2)
        integer, pointer :: chosen => default_target
        integer, pointer :: from_array => numbers(2)
        integer, pointer :: from_member => box%number
        procedure(add_two), nopass, pointer :: callback => add_two
    end type
contains
    integer function add_two(n)
        integer, intent(in) :: n
        add_two = n + 2
    end function
    integer function add_three(n)
        integer, intent(in) :: n
        add_three = n + 3
    end function
    logical function pointer_defaults_restored(x)
        type(child), intent(in) :: x
        pointer_defaults_restored = associated(x%chosen, default_target) .and. &
            associated(x%part(1)%restored, default_target) .and. &
            associated(x%part(2)%restored, default_target) .and. &
            associated(x%from_array, numbers(2)) .and. &
            associated(x%from_member, box%number) .and. &
            associated(x%callback, add_two)
    end function
end module
