program load_model_program
    implicit none

    integer :: int_data1, int_data2
    real :: real_data1, real_data2
    integer :: u = 11

    open(u, file="file_04_data.dat", form="unformatted", access="stream")
    read(u) int_data1, real_data1, int_data2, real_data2

    print *, "Data: ", int_data1, real_data1, int_data2, real_data2
    if (int_data1 /= 345239) error stop
    if (abs(real_data1 - (52.91)) > 1e-6) error stop
    if (int_data2 /= -345239) error stop
    if (abs(real_data2 - (-52.91)) > 1e-6) error stop
end program
