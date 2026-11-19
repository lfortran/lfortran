! procedure(name), where name is declared by a procedure declaration
! statement before the abstract interface it names, in a main program and in
! an external subroutine: an external procedure and a procedure pointer.
subroutine proc_ptr_51_sub(y)
    implicit none
    real, intent(inout) :: y
    procedure(iface) :: ext_scale
    procedure(iface), pointer :: p => null()
    abstract interface
        subroutine iface(x)
            real, intent(inout) :: x
        end subroutine iface
    end interface
    procedure(ext_scale), pointer :: q => null()
    procedure(p), pointer :: r => null()

    q => ext_scale
    call q(y)
    r => ext_scale
    call r(y)
end subroutine proc_ptr_51_sub

subroutine ext_scale(x)
    implicit none
    real, intent(inout) :: x
    x = 2.0*x
end subroutine ext_scale

program proc_ptr_51
    implicit none
    procedure(iface) :: ext_scale
    procedure(iface), pointer :: p => null()
    procedure(p), pointer :: q => null()
    abstract interface
        subroutine iface(x)
            real, intent(inout) :: x
        end subroutine iface
    end interface
    procedure(ext_scale), pointer :: s => null()
    real :: y

    y = 1.0
    q => triple
    call q(y)
    if (abs(y - 3.0) > 1e-6) error stop
    s => ext_scale
    call s(y)
    if (abs(y - 6.0) > 1e-6) error stop
    call proc_ptr_51_sub(y)
    if (abs(y - 24.0) > 1e-6) error stop
    print *, 'ok', y

contains

    subroutine triple(x)
        real, intent(inout) :: x
        x = 3.0*x
    end subroutine triple

end program proc_ptr_51
