program read_character_component_01
    use read_character_component_m
    implicit none
    type(record) :: value

    call parse("AB", value)
    if (value%name /= "AB") error stop
end program
