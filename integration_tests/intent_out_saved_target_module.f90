module intent_out_saved_target_module
    implicit none
    private
    public :: check_saved, reset
contains
    subroutine reset(x)
        class(*), intent(out) :: x
    end subroutine

    subroutine check_saved(expected)
        integer, intent(in) :: expected
        integer, target, save :: target = 31
        type :: saved_item
            integer, pointer :: p => target
        end type
        if (target /= expected) error stop 5
        target = target + 1
    end subroutine
end module
