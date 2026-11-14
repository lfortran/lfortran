program assumed_rank_pointer_associated_01
    implicit none
    integer, pointer :: m(:), m2(:,:), s
    integer, target :: t(3), t2(2,2), ts
    m => null()
    m2 => null()
    s => null()
    if (is_assoc(m)) error stop 1
    if (is_assoc(m2)) error stop 2
    if (is_assoc(s)) error stop 3
    call check_null(m)
    t = [1, 2, 3]
    t2 = 4
    ts = 5
    m => t
    m2 => t2
    s => ts
    if (.not. is_assoc(m)) error stop 4
    if (.not. is_assoc(m2)) error stop 5
    if (.not. is_assoc(s)) error stop 6
    nullify(m)
    if (is_assoc(m)) error stop 7
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
end program
