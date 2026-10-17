program char_array_broadcast_01
    ! Broadcast a non-constant character scalar over character arrays of
    ! fixed and run-time shape, in assignments, comparisons and
    ! concatenations.
    implicit none
    character(len=5) :: a(3), s
    character(len=3) :: b(2, 2)
    character(len=6) :: c(2, 2)
    character(len=:), allocatable :: d(:)
    logical :: m(3)

    s = "hello"
    a = s
    if (any(a /= "hello")) error stop 1
    m = (a == s)
    if (.not. all(m)) error stop 2

    allocate(character(len=5) :: d(4))
    d = s
    if (any(d /= "hello")) error stop 3

    call fill(b, "ab")
    if (any(b /= "ab ")) error stop 4
    c = b // s(1:3)
    if (any(c /= "ab hel")) error stop 5
    c = s(4:5) // b
    if (any(c /= "loab  ")) error stop 6

    call assumed(a, "world")
    if (any(a /= "world")) error stop 7
    call automatic(3, s)
    print *, "ok"

contains

    subroutine fill(x, t)
        character(len=*), intent(out) :: x(:, :)
        character(len=*), intent(in) :: t
        x = t
    end subroutine fill

    subroutine assumed(x, t)
        character(len=*), intent(inout) :: x(:)
        character(len=*), intent(in) :: t
        x = t
        if (any(x /= t)) error stop 8
    end subroutine assumed

    subroutine automatic(n, t)
        integer, intent(in) :: n
        character(len=*), intent(in) :: t
        character(len=len(t)) :: y(n)
        y = t
        if (any(y /= t)) error stop 9
        if (.not. all(y == t)) error stop 10
    end subroutine automatic

end program char_array_broadcast_01
