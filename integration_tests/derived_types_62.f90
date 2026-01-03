module derived_types_62_m
    implicit none
    type :: toml_ser_config
        character(:), allocatable :: indent
        integer, allocatable :: i
    end type toml_ser_config

    type :: toml_serializer
        type(toml_ser_config) :: config = toml_ser_config()
    end type toml_serializer
end module derived_types_62_m


program derived_types_62
    use derived_types_62_m
    type(toml_serializer) :: serializer

    serializer%config%indent = "test"
    if (serializer%config%indent /= "test") error stop

    serializer%config%i = 4
    if (serializer%config%i /= 4) error stop
end program
