program traits_inheritance_05
    use traits_inheritance_05_contracts_m, only: read_value, query
    use traits_inheritance_05_impl_m
    implicit none
    type(RenamedPayload) :: object

    object = RenamedPayload(9)
    if (read_value(object) /= 9) error stop 1
    if (read_value{RenamedPayload}(object) /= 9) error stop 2
    if (query(object) /= 36) error stop 3
    if (query{RenamedPayload}(object) /= 36) error stop 4
end program traits_inheritance_05
