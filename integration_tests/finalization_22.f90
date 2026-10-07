! Entities that exist when the program terminates are not finalized
! (F2018 7.5.6.4): neither the variables of the main program nor those of
! modules, nor the save variables of procedures. The variables of a BLOCK
! construct of the main program are finalized at its END BLOCK, and those of
! a procedure when it returns. A final subroutine called after the end of
! the main program stops with an error.
module finalization_22_mod
    implicit none
    logical :: program_ended = .false.
    integer :: nfin = 0
    type :: t
        integer :: v = 0
    contains
        final :: fin_t
        final :: fin_t1
    end type
    type, extends(t) :: e
    end type
    type :: holder
        type(t) :: c
        type(t), allocatable :: a(:)
    end type
    type(t) :: module_var
contains
    subroutine fin_t(x)
        type(t), intent(inout) :: x
        if (program_ended) error stop 1
        nfin = nfin + 1
    end subroutine

    subroutine fin_t1(x)
        type(t), intent(inout) :: x(:)
        if (program_ended) error stop 2
        nfin = nfin + 1
    end subroutine

    subroutine with_save()
        type(t), save :: s
        s%v = 1
    end subroutine

    subroutine with_locals()
        type(t) :: a
        class(t), allocatable :: c
        a%v = 1
        allocate(e :: c)
    end subroutine

    subroutine end_program()
        program_ended = .true.
    end subroutine
end module

program finalization_22
    use finalization_22_mod
    implicit none
    type(t) :: a, arr(2)
    type(e) :: earr(2)
    type(t), allocatable :: al, ala(:)
    class(t), allocatable :: c, ca(:)
    class(*), allocatable :: u, ua(:)
    type(holder) :: h

    a%v = 1
    arr%v = [2, 3]
    earr%v = [4, 5]
    allocate(al, ala(1))
    allocate(e :: c)
    allocate(e :: ca(2))
    allocate(t :: u)
    allocate(t :: ua(2))
    allocate(h%a(1))
    module_var%v = 6
    call with_save()

    block
        type(t) :: b
        b%v = 7
    end block
    if (nfin /= 1) error stop 3
    call with_locals()
    if (nfin /= 3) error stop 4
    call end_program()
end program
