! DATA initialized vector subscript sections keep their values between calls.
! Not run with GFortran: GFortran 13 ICEs on vector sections in DATA.
program data_29
    implicit none
    integer :: i
    do i = 1, 3
        call vector_sections(i)
    end do
contains
    subroutine vector_sections(call_number)
        integer, intent(in) :: call_number
        integer :: vector(3), matrix(2,3), k
        data vector([3, 1]) /17, 18/
        data matrix(2,[3, 1]) /19, 20/
        k = call_number - 1
        if (any(vector([3, 1]) /= [17, 18] + k)) error stop "vector subscript DATA lifetime"
        if (any(matrix(2,[3, 1]) /= [19, 20] + k)) error stop "matrix vector subscript DATA lifetime"
        vector([3, 1]) = vector([3, 1]) + 1
        matrix(2,[3, 1]) = matrix(2,[3, 1]) + 1
    end subroutine
end program
