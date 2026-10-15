! A procedure pointer component of a named constant is not a variable
! (Fortran 2023 C901): it cannot be a pointer assignment target or be
! called, nor can it be pointer assigned (#14793, #14794). With
! --continue-compilation each such statement is reported and dropped, also
! inside a FORALL construct, and code is still generated for the rest of the
! program.
module cc_pcc_mod
    implicit none
    abstract interface
        integer function ifunc(x)
            integer, intent(in) :: x
        end function
        subroutine isub(x)
            integer, intent(inout) :: x
        end subroutine
    end interface
    type :: pc
        procedure(ifunc), pointer, nopass :: f => null()
        procedure(isub), pointer, nopass :: sp => null()
        integer :: n = 5
    end type
    type :: outer
        type(pc) :: inner
    end type
    type :: dp
        integer, pointer :: p => null()
    end type
    type(outer), parameter :: o = outer(pc(a, s, 4))
    type(pc), parameter :: os(2) = [pc(a, s, 1), pc(a, s, 2)]
    type(dp), parameter :: cs(2) = [dp(null()), dp(null())]
    integer, target :: t = 3
contains
    integer function a(x)
        integer, intent(in) :: x
        a = x + 1
    end function
    subroutine s(x)
        integer, intent(inout) :: x
        x = x + 10
    end subroutine
end module

program continue_compilation_proc_component_constant
    use cc_pcc_mod
    implicit none
    procedure(ifunc), pointer :: q
    type(pc) :: qs(2)
    integer :: i
    i = 1
    q => os(1)%f  ! {Error} pointer assignment target, array element parent
    os(1)%f => a  ! {Error} pointer assignment to an array element parent
    call o%inner%sp(i)  ! {Error} call, nested component parent
    forall (i=1:2)
        qs(i)%f => os(i)%f  ! {Error} pointer assignment target in FORALL
    end forall
    forall (i=1:2)
        cs(i)%p => t  ! {Error} data pointer component of a constant in FORALL
    end forall
    print *, i
end program
