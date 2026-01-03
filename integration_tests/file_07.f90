program file_07
    implicit none

    integer :: u = 11, i, j, k
    integer, allocatable :: int_arr(:, :)
    real, allocatable :: real_arr(:, :, :)
    integer :: int_arr_rows, int_arr_cols, real_arr_rows, real_arr_cols, real_arr_height

    open(u, file="file_07_data.dat", form="unformatted", access="stream", status="old")
    read(u) int_arr_rows, int_arr_cols, real_arr_rows, real_arr_cols, real_arr_height

    print *, int_arr_rows, int_arr_cols, real_arr_rows, real_arr_cols, real_arr_height
    allocate(int_arr(int_arr_rows, int_arr_cols), real_arr(real_arr_rows, real_arr_cols, real_arr_height))
    read(u) int_arr
    read(u) real_arr

    do i = 1, int_arr_rows
        do j = 1, int_arr_cols
            print *, int_arr(i, j)
        end do
    end do

    print *, sum(int_arr)
    if (sum(int_arr) /= 5) error stop

    do i = 1, real_arr_rows
        do j = 1, real_arr_cols
            do k = 1, real_arr_height
                print *, real_arr(i, j, k)
            end do
        end do
    end do

    print *, sum(real_arr)
    if (abs(sum(real_arr) - 1.56999826) > 1e-5) error stop
end program
