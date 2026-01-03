module derived_types_60_mod
    type :: diagnostic_type
        integer :: x
    end type diagnostic_type
    type :: parser_type
      type(diagnostic_type), allocatable :: diagnostic
    end type parser_type
end module derived_types_60_mod

program derived_types_60
    use derived_types_60_mod
    implicit none
    type(diagnostic_type), allocatable :: diagnostic
    type(parser_type) :: parser
    allocate(diagnostic)
    allocate(parser%diagnostic)
    parser%diagnostic%x = 2
    diagnostic = parser%diagnostic
    if (diagnostic%x /= 2) error stop
end program derived_types_60
