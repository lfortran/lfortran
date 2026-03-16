module procedure_45_mod
    implicit none

    abstract interface
        subroutine p_iface(x)
            integer, intent(inout) :: x
        end subroutine p_iface
    end interface

    private :: default_proc
    procedure(p_iface), pointer :: mysub => default_proc

contains

    subroutine default_proc(x)
        integer, intent(inout) :: x
        x = x + 1
    end subroutine default_proc

end module procedure_45_mod

program procedure_45
    use procedure_45_mod
    implicit none

    integer :: x
    x = 41
    call mysub(x)
    if (x /= 42) error stop
end program procedure_45
