! Test sequence association: passing a 2D assumed-size array element
! to a 1D assumed-size dummy argument via sequence association.
program array_section_24
    implicit none
    integer :: arr(2, 3)
    integer :: i, j

    ! Fill with known values: arr(i,j) = i + 10*j
    ! Column-major layout: [11, 21, 12, 22, 13, 23]
    do j = 1, 3
        do i = 1, 2
            arr(i, j) = i + 10 * j
        end do
    end do

    ! caller(arr,1) passes arr(1,1) with n=6: expect [11, 21, 12, 22, 13, 23]
    call caller(arr, 1, 6)
    ! caller(arr,2) passes arr(1,2) with n=4: expect [12, 22, 13, 23]
    call caller(arr, 2, 4)

contains

    subroutine callee(a, n, expected)
        integer, intent(in) :: a(*)
        integer, intent(in) :: n
        integer, intent(in) :: expected(*)
        integer :: k
        do k = 1, n
            if (a(k) /= expected(k)) error stop
        end do
    end subroutine

    subroutine caller(arr, idx, n)
        integer, intent(in) :: idx, n
        integer, intent(in) :: arr(2, *)
        integer :: expected(6)
        integer :: i, j, k
        ! Build expected sequence: column-major from arr(1, idx) onward
        k = 1
        do j = idx, idx + (n + 1) / 2 - 1
            do i = 1, 2
                if (k <= n) then
                    expected(k) = i + 10 * j
                    k = k + 1
                end if
            end do
        end do
        call callee(arr(1, idx), n, expected)
    end subroutine

end program
