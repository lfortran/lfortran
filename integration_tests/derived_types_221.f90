module m_base_221
    implicit none
    type :: base_m
        integer :: m_val = 10
    end type
end module

module m_proc_221
    use m_base_221
    implicit none
    type :: base_in_m
        integer :: b_val = 20
    end type
contains
    subroutine sub_m()
        type, extends(base_in_m) :: ext_in_m
            integer :: ext_b = 21
        end type
        type, extends(base_m) :: ext_from_used
            integer :: ext_u = 11
        end type
        type(ext_in_m) :: x1
        type(ext_from_used) :: x2
        if (x1%b_val /= 20 .or. x1%ext_b /= 21) error stop 1
        if (x2%m_val /= 10 .or. x2%ext_u /= 11) error stop 2
        x1%b_val = 120
        x1%ext_b = 121
        if (x1%b_val /= 120 .or. x1%ext_b /= 121) error stop 3
    end subroutine
end module

program derived_types_221
    use m_proc_221
    use m_base_221, only: base_m
    implicit none

    type :: base_prog
        integer :: p_val = 30
    end type

    call sub_m()
    call sub_internal()

contains

    subroutine sub_internal()
        type, extends(base_prog) :: ext_prog
            integer :: ext_p = 31
        end type
        type, extends(base_m) :: ext_used_in_prog
            integer :: ext_up = 12
        end type
        type(ext_prog) :: y1
        type(ext_used_in_prog) :: y2

        if (y1%p_val /= 30 .or. y1%ext_p /= 31) error stop 4
        if (y2%m_val /= 10 .or. y2%ext_up /= 12) error stop 5
        y1%p_val = 130
        y1%ext_p = 131
        if (y1%p_val /= 130 .or. y1%ext_p /= 131) error stop 6
    end subroutine

end program derived_types_221
