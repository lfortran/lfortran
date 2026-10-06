! An allocatable scalar local is finalized at the end of its procedure by
! the FINAL procedure with a scalar dummy argument: the nonelemental scalar
! one when the type also has a rank-1 FINAL, or an elemental one (#13638).
module finalization_26_mod
    implicit none
    integer :: n0 = 0, n1 = 0, ne = 0, last = 0
    type :: t
        integer :: c = 0
    contains
        final :: fin0
        final :: fin1
    end type
    type :: e
        integer :: c = 0
    contains
        final :: fine
    end type
contains
    ! Defined before the FINAL procedures it relies on.
    subroutine use_t()
        type(t), allocatable :: a
        allocate(a)
        a%c = 3
    end subroutine

    subroutine fin0(self)
        type(t), intent(inout) :: self
        n0 = n0 + 1
        last = self%c
    end subroutine

    subroutine fin1(self)
        type(t), intent(inout) :: self(:)
        n1 = n1 + size(self)
    end subroutine

    impure elemental subroutine fine(self)
        type(e), intent(inout) :: self
        ne = ne + 1
        last = self%c
    end subroutine
end module

program finalization_26
    use finalization_26_mod
    implicit none

    call use_t()
    if (n0 /= 1 .or. n1 /= 0 .or. last /= 3) error stop 1

    call use_e()
    if (ne /= 1 .or. last /= 5) error stop 2
    if (n0 /= 1 .or. n1 /= 0) error stop 3
    print *, n0, n1, ne
contains
    subroutine use_e()
        type(e), allocatable :: x
        allocate(x)
        x%c = 5
    end subroutine
end program
