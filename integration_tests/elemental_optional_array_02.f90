module elemental_optional_array_02_m
    implicit none
    type :: edge_t
        integer :: ivertex = 0
        integer :: n = -1
    end type
contains
    ! every dummy is optional
    impure elemental function ec(ivertex, label) result(e)
        integer, intent(in), optional :: ivertex
        character(len=*), intent(in), optional :: label
        type(edge_t) :: e
        if (present(ivertex)) e%ivertex = ivertex
        if (present(label)) e%n = len(label)
    end function

    impure elemental function ei(i, k) result(r)
        integer, intent(in) :: i
        integer, intent(in), optional :: k
        integer :: r
        r = i
        if (present(k)) r = r + k
    end function

    impure elemental subroutine esub_opt(a, b)
        integer, intent(inout), optional :: a
        integer, intent(in), optional :: b
        if (present(a) .and. present(b)) a = a + b
    end subroutine

    ! the call is guarded by present
    subroutine guarded(ed, label, r)
        integer, intent(in) :: ed(:)
        character(len=*), intent(in), optional :: label(:)
        type(edge_t), intent(out) :: r(:)
        r%ivertex = 0
        if (present(label)) r = ec(ivertex=ed, label=label)
    end subroutine

    ! an array goes to a nonoptional dummy
    subroutine mandatory(ed, k, r)
        integer, intent(in) :: ed(:)
        integer, intent(in), optional :: k(:)
        integer, intent(out) :: r(:)
        r = ei(i=ed, k=k)
    end subroutine

    ! the optional is a scalar
    subroutine scalar_opt(ed, k, r)
        integer, intent(in) :: ed(:)
        integer, intent(in), optional :: k
        integer, intent(out) :: r(:)
        r = ei(i=ed, k=k)
    end subroutine

    subroutine sub_guarded(x, y)
        integer, intent(inout) :: x(:)
        integer, intent(in), optional :: y(:)
        if (present(y)) call esub_opt(a=x, b=y)
    end subroutine

    ! all optional dummies are present
    subroutine all_present(ed, label, r)
        integer, intent(in) :: ed(:)
        character(len=*), intent(in), optional :: label(:)
        type(edge_t), intent(out) :: r(:)
        r = ec(ivertex=ed, label=label)
    end subroutine
end module

program elemental_optional_array_02
    use elemental_optional_array_02_m
    implicit none
    type(edge_t) :: r(2)
    integer :: ri(2), x(2)
    character(len=3) :: lab(2)

    lab = ["abc", "de "]

    call guarded([5, 6], r=r)
    if (any(r%ivertex /= 0)) error stop 1
    call guarded([5, 6], lab, r)
    if (r(1)%ivertex /= 5 .or. r(2)%ivertex /= 6) error stop 2
    if (any(r%n /= 3)) error stop 3

    call mandatory([1, 2], r=ri)
    if (any(ri /= [1, 2])) error stop 4
    call mandatory([1, 2], [10, 20], ri)
    if (any(ri /= [11, 22])) error stop 5

    call scalar_opt([1, 2], r=ri)
    if (any(ri /= [1, 2])) error stop 6
    call scalar_opt([1, 2], 10, ri)
    if (any(ri /= [11, 12])) error stop 7

    x = [1, 2]
    x = [1, 2]
    call sub_guarded(x)
    if (any(x /= [1, 2])) error stop 10
    call sub_guarded(x, [10, 20])
    if (any(x /= [11, 22])) error stop 11

    call all_present([7, 8], lab, r)
    if (r(1)%ivertex /= 7 .or. r(2)%n /= 3) error stop 12
end program
