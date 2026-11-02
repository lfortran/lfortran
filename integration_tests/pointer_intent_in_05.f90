! Optional arrays forwarded to optional pointer, intent(in) dummies of
! functions: the pointer dummy is present exactly when the actual is.
module pointer_intent_in_05_mod
    implicit none
    integer :: calls = 0
contains
    integer function size_or(p)
        integer, pointer, optional, intent(in) :: p(:)
        size_or = -1
        if (present(p)) size_or = size(p)
    end function

    integer function count_present(p, k, q)
        integer, pointer, optional, intent(in) :: p(:), q(:, :)
        integer, intent(in) :: k
        calls = calls + 1
        count_present = k
        if (present(p)) count_present = count_present + 10*size(p)
        if (present(q)) count_present = count_present + 100*size(q)
    end function

    function first_or(p) result(r)
        integer, pointer, optional, intent(in) :: p(:)
        integer :: r(2)
        r = -1
        if (present(p)) r = p(1)
    end function

    integer function len_size_or(p)
        character(len=*), pointer, optional, intent(in) :: p(:)
        len_size_or = -1
        if (present(p)) len_size_or = 100*len(p) + size(p)
    end function

    integer function next()
        calls = calls + 1
        next = calls
    end function

    subroutine forward(n, x, y)
        integer, intent(in) :: n
        integer, optional, target, intent(in) :: x(:)
        integer, optional, target, intent(in) :: y(:, :)
        integer :: r(2), i, s
        logical :: l

        if (size_or(x) /= n) error stop
        if (count_present(x, 1, y) /= expected(1)) error stop
        if (count_present(q=y, k=2) /= expected(2) - 10*max(n, 0)) error stop
        s = size_or(x) + 2*size_or(x)
        if (s /= 3*n) error stop
        l = size_or(x) == n .and. count_present(x, 0, y) == expected(0)
        if (.not. l) error stop

        r = 0
        where (r == 0) r = size_or(x)
        if (any(r /= n)) error stop
        r = 0
        where (r /= 0)
            r = 5
        elsewhere
            r = size_or(x) + count_present(x, 0)
        end where
        if (any(r /= n + count_x())) error stop
        r = first_or(x)
        if (n < 0 .and. any(r /= -1)) error stop
        if (n > 0 .and. any(r /= 7)) error stop

        s = 0
        do i = 1, 3
            s = s + size_or(x)
        end do
        if (s /= 3*n) error stop
        i = 0
        do while (size_or(x) == n .and. i < 4)
            i = i + 1
        end do
        if (i /= 4) error stop

        ! Each reference is evaluated once, with its other actuals
        calls = 0
        s = count_present(x, next(), y)
        if (calls /= 2) error stop
        if (s /= expected(1)) error stop
        if (inner() /= n) error stop
    contains
        integer function expected(k)
            integer, intent(in) :: k
            expected = k + count_x()
            if (present(y)) expected = expected + 100*size(y)
        end function

        integer function count_x()
            count_x = 0
            if (present(x)) count_x = 10*size(x)
        end function

        integer function inner()
            inner = size_or(x)
        end function
    end subroutine

    subroutine forward_character(n, xc)
        integer, intent(in) :: n
        character(len=*), optional, target, intent(in) :: xc(:)
        integer :: r(2)
        if (len_size_or(xc) /= n) error stop
        r = 0
        where (r == 0) r = len_size_or(xc)
        if (any(r /= n)) error stop
    end subroutine
end module

program pointer_intent_in_05
    use pointer_intent_in_05_mod
    implicit none
    integer, target :: a(3), b(2, 2)
    character(len=4), target :: c(2)
    a = 7
    b = 1
    call forward(-1)
    call forward(3, a)
    call forward(-1, y=b)
    call forward(3, a, b)
    c = ['ab  ', 'cd  ']
    call forward_character(-1)
    call forward_character(402, c)
    print *, "ok"
end program
