! F2018 7.5.6.2 step 1: a final subroutine is called for an entity only if
! its dummy argument has the rank of the entity, or else if it is elemental.
! A nonelemental scalar final subroutine is not called for an array.
module finalization_20_mod
    implicit none
    integer :: n_scalar = 0, n_rank1 = 0, n_elem = 0, sum_scalar = 0

    type :: ts
        integer :: c = 0
    contains
        final :: fin_scalar
    end type

    type :: tm
        integer :: c = 0
    contains
        final :: fin_m_scalar
        final :: fin_m_rank1
    end type

    type :: te
        integer :: c = 0
    contains
        final :: fin_elem
    end type

contains

    subroutine fin_scalar(self)
        type(ts), intent(inout) :: self
        n_scalar = n_scalar + 1
        sum_scalar = sum_scalar + self%c
    end subroutine

    subroutine fin_m_scalar(self)
        type(tm), intent(inout) :: self
        n_scalar = n_scalar + 100
    end subroutine

    subroutine fin_m_rank1(self)
        type(tm), intent(inout) :: self(:)
        n_rank1 = n_rank1 + 1
        if (size(self) /= 3) error stop
        if (lbound(self, 1) /= 1) error stop
        if (self(2)%c /= 7) error stop
    end subroutine

    impure elemental subroutine fin_elem(self)
        type(te), intent(inout) :: self
        n_elem = n_elem + 1
    end subroutine

    subroutine scalar_final_on_array()
        type(ts) :: arr(2)
        arr(2)%c = 8
    end subroutine

    subroutine scalar_final_on_scalar()
        type(ts) :: x
        x%c = 5
    end subroutine

    subroutine rank1_final_on_array()
        type(tm) :: arr(3)
        arr(2)%c = 7
    end subroutine

    subroutine no_final_for_rank2()
        type(tm) :: arr(2, 2)
        arr(1, 1)%c = 1
    end subroutine

    subroutine elemental_final_on_array()
        type(te) :: arr(2, 3)
        arr(1, 1)%c = 1
    end subroutine

    subroutine automatic_arrays(n)
        integer, intent(in) :: n
        type(ts) :: as(n)
        type(tm) :: am(n)
        type(te) :: ae(n)
        as(1)%c = 1
        am(2)%c = 7
        ae(1)%c = 1
    end subroutine

end module

program finalization_20
    use finalization_20_mod
    implicit none

    call scalar_final_on_array()
    if (n_scalar /= 0) error stop

    call scalar_final_on_scalar()
    if (n_scalar /= 1) error stop
    if (sum_scalar /= 5) error stop

    call rank1_final_on_array()
    if (n_rank1 /= 1) error stop
    if (n_scalar /= 1) error stop

    call no_final_for_rank2()
    if (n_rank1 /= 1) error stop
    if (n_scalar /= 1) error stop

    call elemental_final_on_array()
    if (n_elem /= 6) error stop

    call automatic_arrays(3)
    if (n_scalar /= 1) error stop
    if (n_rank1 /= 2) error stop
    if (n_elem /= 9) error stop

    print *, n_scalar, n_rank1, n_elem
end program
