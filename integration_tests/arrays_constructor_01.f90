program arrays_constructor_01
    implicit none
    character(5) :: str = "Hello"
    integer :: i = 1

    print *, ["aaa", "aaa"]
    print *, [str(i+1:i+1), str(i:i)]
    print *, ["aaa", str(i+1:i+3), "aaa"]
    print *, [str(i+1:i+3), "aaa"]
end program arrays_constructor_01
