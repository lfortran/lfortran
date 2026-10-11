module bindc_source_value_01_m
    use iso_c_binding, only: c_int
    implicit none
contains
    function source_value(n) result(r)
        integer(c_int), value, intent(in) :: n
        integer(c_int) :: r
        r = n + 19
    end function
    function c_value(n) result(r) bind(c)
        integer(c_int), value, intent(in) :: n
        integer(c_int) :: r
        r = n + 29
    end function
end module

program bindc_source_value_01
    use bindc_source_value_01_m
    implicit none
    if (source_value(4_c_int) /= 23_c_int) error stop 1
    if (c_value(5_c_int) /= 34_c_int) error stop 2
end program
