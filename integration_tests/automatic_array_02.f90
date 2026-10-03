! The associate name of an ASSOCIATE whose selector is an expression with
! the shape of an automatic array has the bounds of the array as evaluated
! on entry, also when the bound variable was redefined since.
module automatic_array_02_m
    implicit none
    integer :: n = 3
contains
    subroutine run()
        real :: a(n)
        integer :: i
        a = [(real(i), i = 1, 3)]
        n = 1
        associate (x => a + 1)
            if (size(x) /= 3) error stop 1
            if (any(x /= [2, 3, 4])) error stop 2
            associate (y => x * 2)
                if (size(y) /= 3) error stop 3
                if (any(y /= [4, 6, 8])) error stop 4
                associate (z => y)
                    if (size(z) /= 3) error stop 5
                    if (any(z + x /= [6, 9, 12])) error stop 6
                end associate
            end associate
            n = 7
            associate (w => -x)
                if (size(w) /= 3) error stop 7
                if (w(3) /= -4) error stop 8
            end associate
        end associate
        associate (v => a)
            associate (u => v - 1)
                if (size(u) /= 3) error stop 9
                if (any(u /= [0, 1, 2])) error stop 10
            end associate
        end associate
    end subroutine

    subroutine run_block(m)
        integer, intent(inout) :: m
        block
            integer :: b(m)
            b = 5
            m = 2
            associate (x => b + 1)
                if (size(x) /= 4) error stop 11
                if (any(x /= 6)) error stop 12
            end associate
        end block
    end subroutine
end module

program automatic_array_02
    use automatic_array_02_m
    implicit none
    integer :: m
    call run()
    m = 4
    call run_block(m)
    print *, "ok"
end program
