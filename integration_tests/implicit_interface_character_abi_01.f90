program implicit_interface_character_abi_01
    implicit none
    integer, external :: character_lengths
    character(len=3), external :: character_result
    character(kind=4, len=3), external :: wide_character_result

    if (character_lengths("A", "BC") /= 12) error stop
    if (character_result("Z") /= "OKZ") error stop
    if (wide_character_result() /= 4_"ABC") error stop
end program
