subroutine initialize_common_character_array()
    implicit none
    character(1) :: value
    character(1) :: initialized(1)
    common /shared_54/ value(1)
    common /initialized_54/ initialized

    if (initialized(1) /= "y") error stop "BLOCK DATA initialization was overwritten"
    value(1) = "x"
    if (value(1) /= "x") error stop "COMMON character array assignment failed"
end subroutine initialize_common_character_array
