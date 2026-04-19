! Test sequence association: passing a 2D assumed-size array element
! to a 1D assumed-size dummy argument via sequence association.
program array_section_24
    implicit none
    integer :: arr(2, 3)
    integer :: i, j

    ! Fill with known values: arr(i,j) = i + 10*j
    do j = 1, 3
        do i = 1, 2
            arr(i, j) = i + 10 * j
        end do
    end do

    call caller(arr, 1)
    call caller(arr, 2)

contains

    subroutine callee(a, n)
        integer, intent(in) :: a(*)
        integer, intent(in) :: n
        integer :: k
        do k = 1, n
            if (a(k) /= a(k)) error stop
        end do
    end subroutine

    subroutine caller(arr, idx)
        integer, intent(in) :: idx
        integer, intent(in) :: arr(2, *)
        ! Sequence association: arr(1, idx) starts a contiguous sequence
        call callee(arr(1, idx), 2 * (3 - idx + 1))
    end subroutine

end program
