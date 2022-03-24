program string_16
    implicit none

    character :: exclamation = achar(33)
    character :: dollar = achar(36)
    character :: left_parenthesis = achar(40)
    character :: plus = achar(43)
    character :: zero = achar(48)
    character :: nine = achar(57)
    character :: semicolon = achar(59)
    character :: capital_a = achar(65)
    character :: capital_z = achar(90)
    character :: right_bracket = achar(93)
    character :: small_a = achar(97)
    character :: small_z = achar(122)
    character :: right_brace = achar(125)

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
