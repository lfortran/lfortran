program string_16
    implicit none
    character, parameter :: exclamation_1 = achar(33)
    character, parameter :: dollar_1 = achar(36)
    character, parameter :: left_parenthesis_1 = achar(40)
    character, parameter :: plus_1 = achar(43)
    character, parameter :: zero_1 = achar(48)
    character, parameter :: nine_1 = achar(57)
    character, parameter :: semicolon_1 = achar(59)
    character, parameter :: capital_a_1 = achar(65)
    character, parameter :: capital_z_1 = achar(90)
    character, parameter :: right_bracket_1 = achar(93)
    character, parameter :: small_a_1 = achar(97)
    character, parameter :: small_z_1 = achar(122)
    character, parameter :: right_brace_1 = achar(125)
    character, parameter :: str_i8 = achar(125_8)

    character, parameter :: ar1(5) = achar([33, 36, 40, 43, 48])
    character, parameter :: ar2(5) = achar([57, 59, 65, 90, 93], 1)

    character :: exclamation
    character :: dollar
    character :: left_parenthesis
    character :: plus
    character :: zero
    character :: nine
    character :: semicolon
    character :: capital_a
    character :: capital_z
    character :: right_bracket
    character :: small_a
    character :: small_z
    character :: right_brace

    integer :: a1 = 35
    integer(8) :: a3 = 12872_8
    integer :: a2(3) = [39, 63, 66]

    exclamation = achar(33)
    dollar = achar(36)
    left_parenthesis = achar(40)
    plus = achar(43)
    zero = achar(48)
    nine = achar(57)
    semicolon = achar(59)
    capital_a = achar(65)
    capital_z = achar(90)
    right_bracket = achar(93)
    small_a = achar(97)
    small_z = achar(122)
    right_brace = achar(125)

    print *, exclamation_1
    if (exclamation_1 /= '!') error stop
    print *, dollar_1
    if (dollar_1 /= '$') error stop
    print *, left_parenthesis_1
    if (left_parenthesis_1 /= '(') error stop
    print *, plus_1
    if (plus_1 /= '+') error stop
    print *, zero_1
    if (zero_1 /= '0') error stop
    print *, nine_1
    if (nine_1 /= '9') error stop
    print *, semicolon_1
    if (semicolon_1 /= ';') error stop
    print *, capital_a_1
    if (capital_a_1 /= 'A') error stop
    print *, capital_z_1
    if (capital_z_1 /= 'Z') error stop
    print *, right_bracket_1
    if (right_bracket_1 /= ']') error stop
    print *, small_a_1
    if (small_a_1 /= 'a') error stop
    print *, small_z_1
    if (small_z_1 /= 'z') error stop
    print *, right_brace_1
    if (right_brace_1 /= '}') error stop
    print *, str_i8
    if (str_i8 /= '}') error stop

    print *, ar1
    if (any(ar1 /= ['!', '$', '(', '+', '0'])) error stop
    print *, ar2
    if (any(ar2 /= ['9', ';', 'A', 'Z', ']'])) error stop

    print*, achar(65)
    if (achar(65) /= 'A') error stop

    print*, achar(a1)
    if (achar(a1) /= '#') error stop

    print*, achar(a2)
    if (achar(a2(1)) /= "'") error stop
    if (achar(a2(2)) /= '?') error stop
    if (achar(a2(3)) /= 'B') error stop

    print *, achar(a3)
    if (achar(a3) /= 'H') error stop

    if(exclamation /= '!') error stop
    if(dollar /= '$') error stop
    if(left_parenthesis /= '(') error stop
    if(plus /= '+') error stop
    if(zero /= '0') error stop
    if(nine /= '9') error stop
    if(semicolon /= ';') error stop
    if(capital_a /= 'A') error stop
    if(capital_z /= 'Z') error stop
    if(right_bracket /= ']') error stop
    if(small_a /= 'a') error stop
    if(small_z /= 'z') error stop
    if(right_brace /= '}') error stop

    print *, exclamation, dollar, left_parenthesis, plus, zero, nine, semicolon
    print *, capital_a, capital_z, right_bracket, small_a, small_z, right_brace

end program
