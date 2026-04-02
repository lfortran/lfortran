program legacy_array_sections_18
    implicit none

    real :: data(4, 4)
    real :: data3(2, 3, 2)
    integer :: i, j
    integer :: k

    do j = 1, 4
        do i = 1, 4
            data(i, j) = 10.0*j + i
        end do
    end do

    do k = 1, 2
        do j = 1, 3
            do i = 1, 2
                data3(i, j, k) = 100.0*k + 10.0*j + i
            end do
        end do
    end do

    call chunk_kernel_4(4, data(:, 1))
    call chunk_kernel_from_3d(data3(:, 1, 2))

contains

    subroutine chunk_kernel_4(n, a)
        integer, value :: n
        real, intent(in) :: a(4, n)

        if (a(1, 1) /= 11.0 .or. a(2, 1) /= 12.0 .or. a(3, 1) /= 13.0 .or. a(4, 1) /= 14.0) then
            error stop "legacy_array_sections_18: column 1 mismatch"
        end if
        if (a(1, 2) /= 21.0 .or. a(2, 2) /= 22.0 .or. a(3, 2) /= 23.0 .or. a(4, 2) /= 24.0) then
            error stop "legacy_array_sections_18: column 2 mismatch"
        end if
        if (a(1, 3) /= 31.0 .or. a(2, 3) /= 32.0 .or. a(3, 3) /= 33.0 .or. a(4, 3) /= 34.0) then
            error stop "legacy_array_sections_18: column 3 mismatch"
        end if
        if (a(1, 4) /= 41.0 .or. a(2, 4) /= 42.0 .or. a(3, 4) /= 43.0 .or. a(4, 4) /= 44.0) then
            error stop "legacy_array_sections_18: column 4 mismatch"
        end if
    end subroutine chunk_kernel_4

    subroutine chunk_kernel_from_3d(a)
        real, intent(in) :: a(2, 3)

        if (a(1, 1) /= 211.0 .or. a(2, 1) /= 212.0) then
            error stop "legacy_array_sections_18: 3d->2d column 1 mismatch"
        end if
        if (a(1, 2) /= 221.0 .or. a(2, 2) /= 222.0) then
            error stop "legacy_array_sections_18: 3d->2d column 2 mismatch"
        end if
        if (a(1, 3) /= 231.0 .or. a(2, 3) /= 232.0) then
            error stop "legacy_array_sections_18: 3d->2d column 3 mismatch"
        end if
    end subroutine chunk_kernel_from_3d

end program legacy_array_sections_18
