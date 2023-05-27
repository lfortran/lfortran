program main
    implicit none

    ! print *, [4, 9]
    ! print *, [[2, -2, 5, 7, [8, -9, [10]]], [3, 3], [-11]]
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

        print *, [[1, a, 2, 3], a, 5, [abs(-2)]]
        print *, "hello", ["hey", ["xyz", ["abc"]]], [[1, a, 2, 3], a, 5, [abs(-2)]], "bye"
        print *, [ 2.1, [3.14, [-5.11, [abs(-21.22), [abs(21.22)]]]] ]
        print *, "Integer(2x2) ArrayConst", [[1, 2], [3, 4]], "Array End"
        print *, "Real(2x2) ArrayVar", b, "Array End"
        print *, "Real(2x2) ArrayConst", [[1.1, -1.2], [2.1, -2.2]], "Array End"
        print *, "Integer(2x2), Real(2x2), ArrayConst", [[1, 2], [3, 4]], [[1.1, -1.2], [2.1, -2.2]], "ArrayEnd"
    end subroutine
end program
