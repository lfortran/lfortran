program char_array_initialization_declaration
    CHARACTER(len=2) :: char1(2) = "ap"
    CHARACTER(len=3), dimension(2) :: char2 = "app"
    CHARACTER(*), dimension(2), parameter :: char1_param = "ap"
    CHARACTER, parameter :: char2_param(2)*3 = "ghi"
    CHARACTER, parameter :: char3_param*(2 + 1) = "App"
    CHARACTER :: char3*4 = "Goat"
    CHARACTER :: char4*4
    
    print *, char1
    if (char1(1) /= "ap") error stop
    if (char1(2) /= "ap") error stop

    print *, char2
    if (char2(1) /= "app") error stop
    if (char2(2) /= "app") error stop

    print *, char1_param
    if (char1_param(1) /= "ap") error stop
    if (char1_param(2) /= "ap") error stop

    print *, char2_param
    if (char2_param(1) /= "ghi") error stop
    if (char2_param(2) /= "ghi") error stop

    print *, char3_param
    if (char3_param /= "App") error stop

    print *, char3
    if (char3 /= "Goat") error stop

    char4 = "Boat"
    print *, char4
    if (char4 /= "Boat") error stop
end program
