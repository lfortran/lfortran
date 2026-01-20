program namelist_3d
    implicit none

    ! Define 3D arrays
    integer :: cube(2,3,4)
    real :: tensor(2,2,2)
    integer :: i, j, k

    ! Define namelist
    namelist /arrays3d/ cube, tensor

    ! Initialize cube (2x3x4 = 24 elements)
    do k = 1, 4
        do j = 1, 3
            do i = 1, 2
                cube(i,j,k) = (i-1)*12 + (j-1)*4 + k
            end do
        end do
    end do

    ! Initialize tensor (2x2x2 = 8 elements)
    tensor(1,1,1) = 1.1
    tensor(1,1,2) = 1.2
    tensor(1,2,1) = 2.1
    tensor(1,2,2) = 2.2
    tensor(2,1,1) = 3.1
    tensor(2,1,2) = 3.2
    tensor(2,2,1) = 4.1
    tensor(2,2,2) = 4.2

    ! Write namelist to file
    open(unit=10, file='namelist_3d.dat', status='replace', form='formatted')
    write(10, nml=arrays3d)
    close(10)

    ! Reset all values to zero
    cube = 0
    tensor = 0.0

    ! Read namelist from file
    open(unit=10, file='namelist_3d.dat', status='old', form='formatted')
    read(10, nml=arrays3d)
    close(10)

    ! Verify cube values
    do k = 1, 4
        do j = 1, 3
            do i = 1, 2
                if (cube(i,j,k) /= (i-1)*12 + (j-1)*4 + k) then
                    error stop "Cube element mismatch"
                end if
            end do
        end do
    end do

    ! Verify tensor values (with tolerance for floating point)
    if (abs(tensor(1,1,1) - 1.1) > 1.0e-5) error stop "tensor(1,1,1) mismatch"
    if (abs(tensor(1,1,2) - 1.2) > 1.0e-5) error stop "tensor(1,1,2) mismatch"
    if (abs(tensor(1,2,1) - 2.1) > 1.0e-5) error stop "tensor(1,2,1) mismatch"
    if (abs(tensor(1,2,2) - 2.2) > 1.0e-5) error stop "tensor(1,2,2) mismatch"
    if (abs(tensor(2,1,1) - 3.1) > 1.0e-5) error stop "tensor(2,1,1) mismatch"
    if (abs(tensor(2,1,2) - 3.2) > 1.0e-5) error stop "tensor(2,1,2) mismatch"
    if (abs(tensor(2,2,1) - 4.1) > 1.0e-5) error stop "tensor(2,2,1) mismatch"
    if (abs(tensor(2,2,2) - 4.2) > 1.0e-5) error stop "tensor(2,2,2) mismatch"

    print *, "3D array namelist test passed!"

end program namelist_3d
