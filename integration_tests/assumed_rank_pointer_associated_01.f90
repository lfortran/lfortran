program assumed_rank_pointer_associated_01
    implicit none
    type :: dt
        integer :: i
    end type
    integer, pointer :: m(:), m2(:,:), s
    integer, target :: t(3), t2(2,2), ts, other(3)
    class(*), pointer :: um(:), us
    class(dt), pointer :: c(:)
    type(dt), target :: td(2)
    m => null()
    m2 => null()
    s => null()
    if (is_assoc(m)) error stop 1
    if (is_assoc(m2)) error stop 2
    if (is_assoc(s)) error stop 3
    call check_null(m)
    if (assoc_with(m, t)) error stop 9
    if (assoc_with_scalar(s, ts)) error stop 10
    t = [1, 2, 3]
    t2 = 4
    ts = 5
    m => t
    m2 => t2
    s => ts
    if (.not. is_assoc(m)) error stop 4
    if (.not. is_assoc(m2)) error stop 5
    if (.not. is_assoc(s)) error stop 6
    if (.not. assoc_with(m, t)) error stop 11
    if (.not. assoc_with2(m2, t2)) error stop 12
    if (.not. assoc_with_scalar(s, ts)) error stop 13
    if (assoc_with(m, other)) error stop 14
    nullify(m)
    if (is_assoc(m)) error stop 7
    if (assoc_with(m, t)) error stop 15

    um => null()
    us => null()
    c => null()
    if (is_assoc_unlimited(um)) error stop 16
    if (is_assoc_unlimited(us)) error stop 17
    if (is_assoc_class(c)) error stop 18
    um => t
    us => ts
    c => td
    if (.not. is_assoc_unlimited(um)) error stop 19
    if (.not. is_assoc_unlimited(us)) error stop 20
    if (.not. is_assoc_class(c)) error stop 21
    nullify(um, c)
    if (is_assoc_unlimited(um)) error stop 22
    if (is_assoc_class(c)) error stop 23
    print *, is_assoc(m), is_assoc(m2), is_assoc(s)
contains
    logical function is_assoc(x)
        integer, pointer, intent(in) :: x(..)
        is_assoc = associated(x)
    end function

    subroutine check_null(q)
        integer, pointer, intent(in) :: q(..)
        if (associated(q)) error stop 8
    end subroutine

    logical function assoc_with(x, y)
        integer, pointer, intent(in) :: x(..)
        integer, target, intent(in) :: y(:)
        assoc_with = associated(x, y)
    end function

    logical function assoc_with2(x, y)
        integer, pointer, intent(in) :: x(..)
        integer, target, intent(in) :: y(:,:)
        assoc_with2 = associated(x, y)
    end function

    logical function assoc_with_scalar(x, y)
        integer, pointer, intent(in) :: x(..)
        integer, target, intent(in) :: y
        assoc_with_scalar = associated(x, y)
    end function

    logical function is_assoc_unlimited(x)
        class(*), pointer, intent(in) :: x(..)
        is_assoc_unlimited = associated(x)
    end function

    logical function is_assoc_class(x)
        class(dt), pointer, intent(in) :: x(..)
        is_assoc_class = associated(x)
    end function
end program
