program test_array_section
    implicit none
    real, allocatable :: arr(:,:)
    real, allocatable :: section(:,:)
    integer :: i, j

    allocate(arr(4,4))
    allocate(section(2,2))
    do j = 1, 4
        do i = 1, 4
            arr(i, j) = real(i + j * 10)
        end do
    end do

    section = arr(2:3, 2:3)

    if (abs(section(1,1) - 22.0) > 0.001) error stop 1
    if (abs(section(2,1) - 23.0) > 0.001) error stop 2
    if (abs(section(1,2) - 32.0) > 0.001) error stop 3
    if (abs(section(2,2) - 33.0) > 0.001) error stop 4

    print *, "PASS"
end program
