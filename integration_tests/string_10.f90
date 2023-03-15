program string_10
    character(len=2) :: c = "BC"
    character(len=3) :: num
    logical :: is_alpha
    is_alpha = (c >= 'A' .and. c <= 'Z') .or. (c >= 'a' .and. c <= 'z')
    print *, is_alpha
    c = "@a"
    is_alpha = (c >= 'A' .and. c <= 'Z') .or. (c >= 'a' .and. c <= 'z')
    print *, is_alpha
    c = "a@"
    is_alpha = (c >= 'A' .and. c <= 'Z') .or. (c >= 'a' .and. c <= 'z')
    print *, is_alpha
    num(1:3) = 'sbs'
    if (num /= "sbs") error stop

end program string_10
