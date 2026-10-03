module metrics_mod_02
    implicit none
    type :: metric_dict_type
        character(10) :: key
        integer :: val
    end type
end module

module network_mod_02
    use metrics_mod_02
    implicit none
    type :: network_type
        type(metric_dict_type), dimension(2) :: metrics
    end type
end module

program derived_component_section_02
    use network_mod_02
    implicit none
    type(network_type) :: net
    character(10) :: arr(2)

    net%metrics(1)%key = "alpha"
    net%metrics(2)%key = "beta"

    arr = net%metrics%key

    if (arr(1) /= "alpha") error stop
    if (arr(2) /= "beta") error stop
    print *, "PASS"
end program
