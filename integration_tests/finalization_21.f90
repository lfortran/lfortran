! F2018 7.5.6.2: an array component whose element type has an elemental or a
! rank-1 final subroutine is finalized, both when the containing object is
! an intent(out) dummy and when it goes out of scope.
module finalization_21_mod
    implicit none
    integer :: n_elem = 0, n_rank1 = 0

    type :: te
        integer :: h = 5
    contains
        final :: fin_elem
    end type

    type :: tr
        integer :: h = 5
    contains
        final :: fin_rank1
    end type

    type :: holder
        type(te) :: ce(2)
        type(tr) :: cr(3)
    end type

contains

    impure elemental subroutine fin_elem(x)
        type(te), intent(inout) :: x
        n_elem = n_elem + 1
    end subroutine

    subroutine fin_rank1(x)
        type(tr), intent(inout) :: x(:)
        n_rank1 = n_rank1 + 10*size(x)
    end subroutine

    subroutine set_out(a)
        type(holder), intent(out) :: a
        a%ce(1)%h = 1
    end subroutine

    subroutine local_holder()
        type(holder) :: b
        b%cr(1)%h = 1
    end subroutine

end module

program finalization_21
    use finalization_21_mod
    implicit none
    type(holder) :: x

    call local_holder()
    print *, n_elem, n_rank1
    if (n_elem /= 2) error stop
    if (n_rank1 /= 30) error stop

    call set_out(x)
    print *, n_elem, n_rank1
    if (n_elem /= 4) error stop
    if (n_rank1 /= 60) error stop
    if (x%ce(1)%h /= 1) error stop
end program
