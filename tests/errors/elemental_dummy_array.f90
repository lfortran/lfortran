module elemental_dummy_array_m
contains
    elemental integer function elemental_dummy_array_f(x)
        integer, intent(in) :: x(3)
        elemental_dummy_array_f = 1
    end function
end module
