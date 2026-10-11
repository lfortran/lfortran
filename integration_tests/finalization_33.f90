! An array of an extended type that has no final subroutine of its own is
! finalized through its parent component, which is an array: an elemental
! final subroutine of the parent type applies to each element, and a
! nonelemental scalar one does not apply (F2018 7.5.6.2).
module finalization_33_mod
    implicit none
    integer :: elemental_calls = 0, elemental_sum = 0, scalar_calls = 0
    type ebase
        integer :: c = 0
    contains
        final :: finish_ebase
    end type
    type, extends(ebase) :: echild
        integer :: z = 0
    end type
    type sbase
        integer :: c = 0
    contains
        final :: finish_sbase
    end type
    type, extends(sbase) :: schild
        integer :: z = 0
    end type
contains
    impure elemental subroutine finish_ebase(self)
        type(ebase), intent(inout) :: self
        elemental_calls = elemental_calls + 1
        elemental_sum = elemental_sum + self%c
    end subroutine
    subroutine finish_sbase(self)
        type(sbase), intent(inout) :: self
        scalar_calls = scalar_calls + 1
    end subroutine
    subroutine local_arrays()
        type(echild) :: a(2)
        type(schild) :: b(3)
        a%c = 3
        b%c = 1
    end subroutine
end module

program finalization_33
    use finalization_33_mod
    implicit none
    type(echild), allocatable :: e(:)
    type(schild), allocatable :: s(:)

    call local_arrays()
    print *, elemental_calls, elemental_sum, scalar_calls
    if (elemental_calls /= 2 .or. elemental_sum /= 6) error stop 1
    if (scalar_calls /= 0) error stop 2

    allocate(e(3))
    e%c = 4
    deallocate(e)
    print *, elemental_calls, elemental_sum, scalar_calls
    if (elemental_calls /= 5 .or. elemental_sum /= 18) error stop 3

    allocate(s(2))
    deallocate(s)
    print *, elemental_calls, elemental_sum, scalar_calls
    if (scalar_calls /= 0) error stop 4
end program
