module stdlib_linalg
    implicit none

    ! Define the generic interface 'diag'
    interface diag
        procedure :: diag_iint16_mat  ! Handles matrix to vector
        procedure :: diag_iint16_vec  ! Handles vector to matrix
    end interface

contains

    ! Function to extract the diagonal of a matrix
    module function diag_iint16_mat(A) result(res)
        integer(4), intent(in) :: A(:,:)
        integer(4) :: res(minval(shape(A)))
        integer :: i

        do i = 1, minval(shape(A))
            res(i) = A(i, i)
        end do
    end function diag_iint16_mat

    ! Function to create a diagonal matrix from a vector
    module function diag_iint16_vec(v) result(res)
        integer(4), intent(in) :: v(:)
        integer(4) :: res(size(v), size(v))
        integer :: i, j

        ! Initialize the result matrix to zero
        res = 0

        ! Assign the vector elements to the diagonal of the matrix
        do i = 1, size(v)
            res(i, i) = v(i)
        end do
    end function diag_iint16_vec

    ! Test subroutine to demonstrate diag(diag(a))
    subroutine test_diag_int16()
        integer, parameter :: n = 4
        integer(4) :: a(n, n), d(n**2)
        integer(4) :: e(n, n)
        integer :: j

        ! Initialize the array 'd' with values from 1 to n^2
        do j = 1, n**2
            d(j) = j
        end do

        ! Reshape 'd' into an n x n matrix 'a'
        a = reshape(d, [n, n])

        ! Apply diag twice: first to extract the diagonal vector,
        ! then to create a diagonal matrix
        e = diag(diag(a))

        ! Optional: Print the results for verification
        print *, "Matrix a:"
        do j = 1, n
            print *, a(j, :)
        end do

        print *, "Diagonal of a:"
        print *, diag(a)
        if (any(diag(a) /= [1, 6, 11, 16])) error stop

        print *, "diag(diag(a)):"
        print *, e(1, :)
        if (any(e(1, :) /= [1, 0, 0, 0])) error stop
        print *, e(2, :)
        if (any(e(2, :) /= [0, 6, 0, 0])) error stop
        print *, e(3, :)
        if (any(e(3, :) /= [0, 0, 11, 0])) error stop
        print *, e(4, :)
        if (any(e(4, :) /= [0, 0, 0, 16])) error stop
    end subroutine test_diag_int16

end module stdlib_linalg

program main
    use stdlib_linalg
    implicit none

    call test_diag_int16()
end program main
