module block_24_m
    implicit none
contains
    subroutine check(k)
        integer, intent(in) :: k
        integer :: y
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
            if (letters(k) /= "abcdefgh"(1:k)) error stop
            call opt(y)
            if (y /= -1) error stop
            call opt(y, k)
            if (y /= k) error stop
        end block
    end subroutine check
end module block_24_m

program block_24
    ! The external procedures, defined in block_24_procs.f90, are declared
    ! only by interface blocks in BLOCK constructs. Their interfaces are
    ! transformed by the compiler: array, allocatable and character(len=n)
    ! results, and optional and assumed-shape dummy arguments.
    use block_24_m, only: check
    implicit none
    integer :: x(3), y
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

            subroutine opt(a, b)
                integer, intent(out) :: a
                integer, intent(in), optional :: b
            end subroutine opt

            integer function total(a)
                integer, intent(in) :: a(:)
            end function total
        end interface
        x = iota(3)
        if (any(x /= [1, 2, 3])) error stop
        if (total(x) /= 6) error stop
        x = filled(4)
        if (any(x /= 4)) error stop
        call opt(y, 5)
        if (y /= 5) error stop
    end block
    call check(3)
    print *, x, y
end program block_24
