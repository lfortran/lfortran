program array_indices_array_item_assignment_4
    use iso_fortran_env, only: real32
    implicit none

    real(real32), allocatable, dimension(:,:) :: right_data
    real(real32), dimension(3,3) :: data_copy

    integer :: i, j
    integer, dimension(2) :: idx_a, idx_b

    do i = 1, 3
       do j = 1, 3
          data_copy(i,j) = real(i*10 + j, real32)
       end do
    end do

    idx_a = [1, 2]
    idx_b = [1, 3]

    right_data = data_copy(idx_a, idx_b)

    if (size(right_data, 1) /= 2 .or. size(right_data, 2) /= 2) error stop "wrong shape"
    if (abs(right_data(1,1) - 11.0_real32) > 1e-4) error stop "right_data(1,1) wrong"
    if (abs(right_data(2,1) - 21.0_real32) > 1e-4) error stop "right_data(2,1) wrong"
    if (abs(right_data(1,2) - 13.0_real32) > 1e-4) error stop "right_data(1,2) wrong"
    if (abs(right_data(2,2) - 23.0_real32) > 1e-4) error stop "right_data(2,2) wrong"
end program array_indices_array_item_assignment_4
