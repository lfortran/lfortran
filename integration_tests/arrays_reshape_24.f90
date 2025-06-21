module regex_test_1_arrays_reshape_24
   !  use regex_module
    implicit none
    public
    character(len=*), parameter :: test1data(4,1) = reshape([ character(len=30) :: &
        "YES", "\d                           ", "5                             ", "1 "],[4,1])
end module regex_test_1_arrays_reshape_24

program arrays_reshape_24
    use regex_test_1_arrays_reshape_24
    implicit none
    print *, test1data
    if (test1data(1,1) /= "YES") error stop
    if (test1data(2,1) /= "\d                           ") error stop
    if (test1data(3,1) /= "5                             ") error stop
    if (test1data(4,1) /= "1                             ") error stop
end program
