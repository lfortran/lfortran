program test_char_kind_len
    implicit none
    character(kind=1, len=10) :: str1
    character(len=10, kind=1) :: str2
    character(10, 1) :: str3
    character(10, kind=1) :: str4

    str1 = "Hello1"
    str2 = "Hello2"
    str3 = "Hello3"
    str4 = "Hello4"
    print *, "str1 = ", str1, ", length = ", len(str1)
    print *, "str2 = ", str2, ", length = ", len(str2)
    print *, "str3 = ", str3, ", length = ", len(str3)
    print *, "str4 = ", str4, ", length = ", len(str4)
end program test_char_kind_len
