! Test read into allocatable-arrays, for integers and real

program read_63
    implicit none

    integer, parameter :: n_lines = 3, n_cols = 3
    integer :: i
    
    ! Integer variables
    integer, dimension(:,:), allocatable :: array
    integer :: expected(n_lines, n_cols)
    character(len=n_cols), dimension(n_lines) :: file_str = ['890', '781', '874']

    ! Real variables
    real, dimension(:,:), allocatable :: r_array
    real :: r_expected(n_lines, n_cols)
    character(len=n_cols), dimension(n_lines) :: r_file_str = ['123', '456', '789']

    ! --- Integer Logic ---
    expected(1,:) = [8, 9, 0]
    expected(2,:) = [7, 8, 1]
    expected(3,:) = [8, 7, 4]

    allocate(array(n_lines, n_cols))
    do i = 1, n_lines
        read(file_str(i), '(*(I1))') array(i, :)
        print *, "Int Row ", i, ": ", array(i, :)
    end do

    if (any(array /= expected)) error stop

    ! --- Real Logic ---
    r_expected(1,:) = [1.0, 2.0, 3.0]
    r_expected(2,:) = [4.0, 5.0, 6.0]
    r_expected(3,:) = [7.0, 8.0, 9.0]

    allocate(r_array(n_lines, n_cols))
    do i = 1, n_lines
        read(r_file_str(i), '(*(F1.0))') r_array(i, :)
        print *, "Real Row ", i, ": ", r_array(i, :)
    end do

    if (any(abs(r_array - r_expected) > 1e-4)) error stop
    
end program read_63