program traits_static_bindc_01
    use traits_static_bindc_01_m
    implicit none
    type(SourcePayload) :: source_object
    if (source_object%value(4) /= 23) error stop 1
end program
