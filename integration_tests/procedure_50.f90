module procedure_50_mod
    implicit none
    procedure(set_value), pointer :: test => set_value
contains
    subroutine set_value(x)
        integer, intent(inout) :: x
        x = x + 42
    end subroutine set_value
end module procedure_50_mod

program procedure_50
    use procedure_50_mod
    implicit none
    integer :: x
    x = 0
    call test(x)
    if (x /= 42) error stop
end program procedure_50
