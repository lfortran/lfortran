program file_03
    implicit none

    integer :: no_of_students, roll_no, age, fd, i
    character(len = 20) :: first_name, last_name
    real :: weight

    ! for asserting/verification
    integer :: total_age = 0
    real :: total_weight = 0.0

    fd = 10
    open(fd, file="file_03_data.txt")
    read(fd, *) no_of_students

    print *, "Total no of students is:", no_of_students

    do i = 1, no_of_students
        read(fd, *) roll_no, first_name, last_name, age, weight
        print *, roll_no, first_name, last_name, age, weight

        total_age = total_age + age
        total_weight = total_weight + weight
    end do

    print *, "total_age:", total_age
    print *, "total_weight:", total_weight

    if (total_age /= 150) error stop
    if (abs(total_weight - 519.600037) > 1e-6) error stop
end program
