program test_char_len
    implicit none
    character(len=10) :: str1
    character(10) :: str2

    str1 = "Hello"
    str2 = "World"
    print *, "str1 = ", str1, ", length = ", len(str1)
    print *, "str2 = ", str2, ", length = ", len(str2)
end program test_char_len
