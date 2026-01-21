program namelist_mixed
    implicit none

    ! Define variables of mixed dimensions
    integer :: scalar
    integer :: vec(5)
    integer :: mat(3,3)
    integer :: cube(2,2,2)
    integer :: i, j, k

    ! Define namelist with all dimensions mixed together
    namelist /mixed/ scalar, vec, mat, cube

    ! Initialize scalar
    scalar = 42

    ! Initialize vector (5 elements)
    vec = [10, 20, 30, 40, 50]

    ! Initialize matrix (3x3 = 9 elements)
    do j = 1, 3
        do i = 1, 3
            mat(i,j) = i + j * 10
        end do
    end do

    ! Initialize cube (2x2x2 = 8 elements)
    do k = 1, 2
        do j = 1, 2
            do i = 1, 2
                cube(i,j,k) = i + j*10 + k*100
            end do
        end do
    end do

    ! Write namelist to file
    open(unit=10, file='namelist_mixed.dat', status='replace', form='formatted')
    write(10, nml=mixed)
    close(10)

    ! Reset all values to zero
    scalar = 0
    vec = 0
    mat = 0
    cube = 0

    ! Read namelist from file
    open(unit=10, file='namelist_mixed.dat', status='old', form='formatted')
    read(10, nml=mixed)
    close(10)

    ! Verify scalar
    if (scalar /= 42) error stop "Scalar mismatch"

    ! Verify vector
    if (vec(1) /= 10) error stop "vec(1) mismatch"
    if (vec(2) /= 20) error stop "vec(2) mismatch"
    if (vec(3) /= 30) error stop "vec(3) mismatch"
    if (vec(4) /= 40) error stop "vec(4) mismatch"
    if (vec(5) /= 50) error stop "vec(5) mismatch"

    ! Verify matrix
    do j = 1, 3
        do i = 1, 3
            if (mat(i,j) /= i + j * 10) then
                error stop "Matrix element mismatch"
            end if
        end do
    end do

    ! Verify cube
    do k = 1, 2
        do j = 1, 2
            do i = 1, 2
                if (cube(i,j,k) /= i + j*10 + k*100) then
                    error stop "Cube element mismatch"
                end if
            end do
        end do
    end do

    print *, "Mixed dimension namelist test passed!"

end program namelist_mixed
