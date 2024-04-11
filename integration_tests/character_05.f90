program character_05
    implicit none
    !> we can specify character length as an expression "2 + 1"
    CHARACTER, parameter :: char_param*(2 + 1) = "App"
    CHARACTER :: char1*4 = "Goat"
    CHARACTER :: char2*4

    if (char_param /= "App") error stop
    if (char1 /= "Goat") error stop

    ! this should raise a warning, currently it doesn't
    char1 = "Apple"
    if (char1 /= "Appl") error stop

    char2 = "Goat"
    if (char2 /= "Goat") error stop

    ! this should raise a warning, currently it doesn't
    char2 = "Balle"
    if (char2 /= "Ball") error stop
end program
