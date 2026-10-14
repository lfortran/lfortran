module block_23_m
    implicit none
contains
    integer function msum(k)
        integer, intent(in) :: k
        integer :: x(3), y
        block
            interface
                function iota(n) result(r)
                    integer, intent(in) :: n
                    integer :: r(n)
                end function iota

                subroutine opt(a, b)
                    integer, intent(out) :: a
                    integer, intent(in), optional :: b
                end subroutine opt

                integer function total(a)
                    integer, intent(in) :: a(:)
                end function total
            end interface
            x = iota(3)
            call opt(y, k)
            msum = total(x) + y
        end block
    end function msum
end module block_23_m

program block_23
    ! Interface blocks in BLOCK constructs declare external procedures whose
    ! interfaces the compiler transforms: array, allocatable and
    ! character(len=n) results, optional and assumed-shape dummy arguments,
    ! and a character(len=*) dummy argument.
    use block_23_m, only: msum
    implicit none
    integer :: x(3), y
    x = 0
    block
        interface
            function iota(n) result(r)
                integer, intent(in) :: n
                integer :: r(n)
            end function iota

            function filled(a) result(r)
                integer, intent(in) :: a
                integer, allocatable :: r(:)
            end function filled

            function letters(n) result(r)
                integer, intent(in) :: n
                character(len=n) :: r
            end function letters

            subroutine opt(a, b)
                integer, intent(out) :: a
                integer, intent(in), optional :: b
            end subroutine opt

            integer function total(a)
                integer, intent(in) :: a(:)
            end function total

            integer function length(s)
                character(len=*), intent(in) :: s
            end function length
        end interface
        x = iota(3)
        if (any(x /= [1, 2, 3])) error stop
        if (total(x) /= 6) error stop
        x = filled(4)
        if (any(x /= 4)) error stop
        if (letters(4) /= "abcd") error stop
        if (len(letters(2)) /= 2) error stop
        call opt(y)
        if (y /= -1) error stop
        call opt(y, 5)
        if (y /= 5) error stop
        if (length("hello") /= 5) error stop
        block
            if (total(iota(4)) /= 10) error stop
            call opt(y, 7)
            if (y /= 7) error stop
        end block
    end block
    call internal()
    if (msum(8) /= 14) error stop
    print *, x, y
contains
    subroutine internal()
        block
            interface
                function letters(n) result(r)
                    integer, intent(in) :: n
                    character(len=n) :: r
                end function letters

                subroutine opt(a, b)
                    integer, intent(out) :: a
                    integer, intent(in), optional :: b
                end subroutine opt
            end interface
            if (letters(3) /= "abc") error stop
            call opt(y)
            if (y /= -1) error stop
        end block
    end subroutine internal
end program block_23

function iota(n) result(r)
    implicit none
    integer, intent(in) :: n
    integer :: r(n), i
    r = [(i, i = 1, n)]
end function iota

function filled(a) result(r)
    implicit none
    integer, intent(in) :: a
    integer, allocatable :: r(:)
    allocate(r(3))
    r = a
end function filled

function letters(n) result(r)
    implicit none
    integer, intent(in) :: n
    character(len=n) :: r
    r = "abcdefgh"(1:n)
end function letters

subroutine opt(a, b)
    implicit none
    integer, intent(out) :: a
    integer, intent(in), optional :: b
    if (present(b)) then
        a = b
    else
        a = -1
    end if
end subroutine opt

integer function total(a)
    implicit none
    integer, intent(in) :: a(:)
    total = sum(a)
end function total

integer function length(s)
    implicit none
    character(len=*), intent(in) :: s
    length = len(s)
end function length
