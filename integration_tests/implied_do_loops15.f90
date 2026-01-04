program implied_do_loops15
integer n(3)
n = [(42, i = 1, size(n))]
print "(3I3)", n
if (any(n /= 42)) error stop

call test_expression()
call test_subroutine()

contains
    subroutine test_expression()
        integer m(5)
        m = [(i*2, i = 1, 5)]
        print "(5I3)", m
        if (m(1) /= 2) error stop
        if (m(5) /= 10) error stop
    end subroutine test_expression

    subroutine test_subroutine()
        integer arr(4)
        arr = [(j*3, j = 1, 4)]
        print "(4I3)", arr
        if (arr(1) /= 3) error stop
        if (arr(4) /= 12) error stop
    end subroutine test_subroutine
end program implied_do_loops15
