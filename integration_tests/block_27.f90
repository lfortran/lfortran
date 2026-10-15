module block_27_m
    implicit none
    integer :: n = 0
    type :: pair
        integer :: a, b
    end type pair
contains
    subroutine module_work()
        integer :: z(3)
        z = [7, 8, 9]
        block
            interface
                integer function total(a)
                    import :: n
                    integer, intent(in) :: a(n)
                end function total
            end interface
            if (total(z) /= 24) error stop
        end block
    end subroutine module_work
end module block_27_m

program block_27
    ! Interface bodies in BLOCK constructs give the bounds of a dummy
    ! argument by a module variable that the external procedure uses too.
    ! The variable is imported from the program, from the host of an
    ! internal procedure or from the module of a module procedure, or
    ! accessed by a USE statement in the interface body.
    use block_27_m, only: n, pair, module_work
    implicit none
    integer :: x(3), y(4)
    type(pair) :: p
    n = 3
    x = [1, 2, 3]
    block
        interface
            integer function total(a)
                import :: n
                integer, intent(in) :: a(n)
            end function total

            function make_pair(i) result(r)
                import :: pair
                integer, intent(in) :: i
                type(pair) :: r
            end function make_pair
        end interface
        if (total(x) /= 6) error stop
        p = make_pair(4)
        if (p%a /= 4 .or. p%b /= 8) error stop
    end block
    block
        interface
            subroutine iota(i, a)
                use block_27_m, only: n
                integer, intent(in) :: i
                integer, intent(out) :: a(n + 1)
            end subroutine iota
        end interface
        call iota(5, y)
        if (any(y /= [5, 6, 7, 8])) error stop
    end block
    call work()
    call module_work()
    print *, "ok"
contains
    subroutine work()
        integer :: z(3)
        z = [4, 5, 6]
        block
            interface
                integer function total(a)
                    import :: n
                    integer, intent(in) :: a(n)
                end function total
            end interface
            if (total(z) /= 15) error stop
        end block
    end subroutine work
end program block_27

integer function total(a)
    use block_27_m, only: n
    implicit none
    integer, intent(in) :: a(n)
    total = sum(a)
end function total

subroutine iota(i, a)
    use block_27_m, only: n
    implicit none
    integer, intent(in) :: i
    integer, intent(out) :: a(n + 1)
    integer :: j
    a = [(i + j - 1, j = 1, n + 1)]
end subroutine iota

function make_pair(i) result(r)
    use block_27_m, only: pair
    implicit none
    integer, intent(in) :: i
    type(pair) :: r
    r%a = i
    r%b = 2*i
end function make_pair
