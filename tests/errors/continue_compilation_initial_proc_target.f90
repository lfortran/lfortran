! An internal procedure cannot be an initial procedure target (Fortran 2023
! 15.4.3.6 C1519): neither in the initialization of a procedure pointer or of
! a procedure pointer component (7.5.4.6), nor as the value of such a
! component in a structure constructor of an initialization (10.1.12 (3)(b)),
! whether the internal procedure is defined before or after its use
! (#14968). Module and external procedures are valid initial targets, and an
! internal procedure is a valid target at run time (C1033).
module cc_ipt_mod
    implicit none
    abstract interface
        subroutine cc_ipt_iface(i)
            integer, intent(in) :: i
        end subroutine
    end interface
    type :: cc_ipt_t
        procedure(cc_ipt_iface), pointer, nopass :: f
    end type
contains
    subroutine cc_ipt_mod_proc(i)
        integer, intent(in) :: i
    end subroutine
    subroutine cc_ipt_host()
        type(cc_ipt_t), parameter :: x = cc_ipt_t(cc_ipt_internal)  ! {Error} internal procedure of a module procedure
    contains
        subroutine cc_ipt_internal(i)
            integer, intent(in) :: i
        end subroutine
    end subroutine
end module

program continue_compilation_initial_proc_target
    use cc_ipt_mod, only: cc_ipt_t, cc_ipt_mod_proc
    implicit none
    abstract interface
        subroutine ffunc(i)
            integer, intent(in) :: i
        end subroutine
    end interface
    interface
        subroutine cc_ipt_ext(i)
            integer, intent(in) :: i
        end subroutine
    end interface
    type :: callback
        procedure(ffunc), pointer, nopass :: f
    end type
    type :: callback_a
        procedure(a), pointer, nopass :: f
    end type
    type :: default_a
        procedure(a), pointer, nopass :: f => a  ! {Error} component initialization
    end type
    type :: default_s
        procedure(ffunc), pointer, nopass :: f => a  ! {Error} component initialization
    end type
    type :: holder
        type(callback) :: c = callback(a)  ! {Error} component default initialization
    end type
    type(callback), parameter :: my_f = callback(a)  ! {Error} named constant
    type(callback_a), parameter :: my_a = callback_a(a)  ! {Error} named constant
    type(callback) :: v = callback(f=a)  ! {Error} variable initialization
    procedure(a), pointer :: q => a  ! {Error} procedure pointer initialization
    procedure(ffunc), pointer :: r => a  ! {Error} procedure pointer initialization
    type(callback), parameter :: by_module = callback(cc_ipt_mod_proc)
    type(callback), parameter :: by_external = callback(cc_ipt_ext)
    procedure(ffunc), pointer :: s => cc_ipt_ext
    type(callback) :: w
    w = callback(a)
    w%f => a
    call call_f(my_f)
    call call_a(my_a)
    block
        type(callback), parameter :: in_block = callback(a)  ! {Error} named constant of a block
    end block
contains
    subroutine a(i)
        integer, intent(in) :: i
        type(cc_ipt_t), parameter :: later = cc_ipt_t(b)  ! {Error} internal procedure defined later
    end subroutine
    subroutine b(i)
        integer, intent(in) :: i
        type(cc_ipt_t), parameter :: earlier = cc_ipt_t(a)  ! {Error} internal procedure defined earlier
    end subroutine
    subroutine call_f(x)
        type(callback), intent(in) :: x
        call x%f(5)
    end subroutine
    subroutine call_a(x)
        type(callback_a), intent(in) :: x
        call x%f(5)
    end subroutine
end program
