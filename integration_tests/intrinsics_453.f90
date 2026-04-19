program intrinsics_453
    implicit none

    type CFG_var_t
        character(len=100) :: var_name
    end type CFG_var_t

    type(CFG_var_t) :: list(2)
    integer :: marker

    list(1)%var_name = "zeta"
    list(2)%var_name = "alpha"

    call partition_var_list(list, marker)

    if (marker /= 2) error stop

contains

    subroutine partition_var_list(list, marker)
        type(CFG_var_t), intent(inout) :: list(:)
        integer, intent(out)           :: marker
        integer                        :: right
        character(len=100)             :: pivot_value

        right = size(list)
        pivot_value = list(1)%var_name

        do while (lgt(list(right)%var_name, pivot_value))
            right = right - 1
            if (right < 1) error stop
        end do

        marker = right
    end subroutine partition_var_list

end program intrinsics_453
