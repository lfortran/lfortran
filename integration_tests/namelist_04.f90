program namelist_2d
    implicit none

    ! Define 2D arrays
    integer :: matrix(3,4)
    real :: grid(2,3)
    integer :: i, j

    ! Define namelist
    namelist /arrays2d/ matrix, grid

    ! Initialize matrix (3x4 = 12 elements)
    do j = 1, 4
        do i = 1, 3
            matrix(i,j) = (i-1)*4 + j
        end do
    end do

    ! Initialize grid (2x3 = 6 elements)
    grid(1,1) = 1.5
    grid(1,2) = 2.5
    grid(1,3) = 3.5
    grid(2,1) = 4.5
    grid(2,2) = 5.5
    grid(2,3) = 6.5

    ! Write namelist to file
    open(unit=10, file='namelist_2d.dat', status='replace', form='formatted')
    write(10, nml=arrays2d)
    close(10)

    ! Reset all values to zero
    matrix = 0
    grid = 0.0

    ! Read namelist from file
    open(unit=10, file='namelist_2d.dat', status='old', form='formatted')
    read(10, nml=arrays2d)
    close(10)

    ! Verify matrix values
    do j = 1, 4
        do i = 1, 3
            if (matrix(i,j) /= (i-1)*4 + j) error stop "Matrix element mismatch"
        end do
    end do

    ! Verify grid values (with tolerance for floating point)
    if (abs(grid(1,1) - 1.5) > 1.0e-5) error stop "grid(1,1) mismatch"
    if (abs(grid(1,2) - 2.5) > 1.0e-5) error stop "grid(1,2) mismatch"
    if (abs(grid(1,3) - 3.5) > 1.0e-5) error stop "grid(1,3) mismatch"
    if (abs(grid(2,1) - 4.5) > 1.0e-5) error stop "grid(2,1) mismatch"
    if (abs(grid(2,2) - 5.5) > 1.0e-5) error stop "grid(2,2) mismatch"
    if (abs(grid(2,3) - 6.5) > 1.0e-5) error stop "grid(2,3) mismatch"

    print *, "2D array namelist test passed!"

end program namelist_2d
