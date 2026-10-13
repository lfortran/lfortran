program traits_separate_01
    use traits_separate_01_contracts_m, only: read_value
    use traits_separate_01_impl_m
    implicit none
    type(Payload) :: object
    object = Payload(23)
    if (read_value(object) /= 23) error stop
    if (read_value{Payload}(object) /= 23) error stop
end program traits_separate_01
