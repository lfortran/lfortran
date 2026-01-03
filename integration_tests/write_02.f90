program main
    implicit none

    write (*, *), [4, 9]
    write (*, *), [[2, -2, 5, 7, [8, -9, [10]]], [3, 3], [-11]]
    call f()

    contains

    subroutine f()
        integer, allocatable :: a(:, :)
        real :: b(2, 2)
        integer :: i, j
        allocate(a(5, 10))

        do i = lbound(a, 1), ubound(a, 1)
            do j = lbound(a, 2), ubound(a, 2)
                a(i, j) = i + j
            end do
        end do

        b(1, 1) = 1.1
        b(1, 2) = -1.2
        b(2, 1) = 2.1
        b(2, 2) = -2.2

        write (*, *), [[1, a, 2, 3], a, 5, [abs(-2)]]
        write (*, *), "hello", ["hey", ["xyz", ["abc"]]], [[1, a, 2, 3], a, 5, [abs(-2)]], "bye"
        write (*, *), [2.1, [3.14, [-5.11, [abs(-21.22), [abs(21.22)]]]]]
        write (*, *), "Integer(2x2) ArrayConst", [[1, 2], [3, 4]], "Array End"
        write (*, *), "Real(2x2) ArrayVar", b, "Array End"
        write (*, *), "Real(2x2) ArrayConst", [[1.1, -1.2], [2.1, -2.2]], "Array End"
        write (*, *), "Integer(2x2), Real(2x2), ArrayConst", [[1, 2], [3, 4]], [[1.1, -1.2], [2.1, -2.2]], "ArrayEnd"
    end subroutine
end program
