block data initialize_common_character_data
    implicit none
    character(1) :: initialized(1)
    common /initialized_52/ initialized
    data initialized /"y"/
end block data initialize_common_character_data
