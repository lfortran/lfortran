program legacy_array_sections_08
    implicit none
    real(4) :: x(2)
    x = [1.0, 2.0]
    print *, f1(x)
    if (any(f1(x) - [0.841470957, 0.909297407] > 1e-6)) error stop
    contains
        pure elemental real(4) function f1(x) 
            implicit none
            real(4),intent(in) :: x
            f1 = sin(x)
        end function f1
end program legacy_array_sections_08
