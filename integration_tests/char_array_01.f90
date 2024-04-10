program char_array_01
    character char_arr1(4)
    character char_arr2(2)
    char_arr1(:) = "A"
    char_arr2(:) = " "

    print *, char_arr1
    if (char_arr1(1) /= "A") error stop
    if (char_arr1(2) /= "A") error stop
    if (char_arr1(3) /= "A") error stop
    if (char_arr1(4) /= "A") error stop

    print *, char_arr2
    if (char_arr2(1) /= " ") error stop
    if (char_arr2(2) /= " ") error stop
end program char_array_01
