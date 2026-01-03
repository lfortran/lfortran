! This program demonstrates the use of the ISO C binding to call a C function from Fortran.
! It tests the linking of Fortran and C object files in different orders.
! The Fortran program defines an interface for the C 'add' function, which adds two integers.
! The test cases ensure the correct compilation and execution, regardless of the linking order
! between the Fortran and C files, as shown in the corresponding test script.
program unordered_linking
    use iso_c_binding, only: c_int
    implicit none

    interface
        function add(a, b) bind(C)
            import :: c_int
            integer(c_int) :: add
            integer(c_int), value :: a, b
        end function add
    end interface

    integer(c_int) :: a, b, result

    a = 5
    b = 3
    result = add(a, b)
    print *, 'The sum is: ', result
end program unordered_linking
