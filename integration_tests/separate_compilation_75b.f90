module separate_compilation_75b
    use, intrinsic :: ieee_arithmetic, only: ieee_copy_sign, ieee_value, &
        ieee_negative_normal, ieee_class, ieee_class_type, operator(==)
    implicit none
contains
    real function neg_b(x)
        real, intent(in) :: x
        neg_b = ieee_copy_sign(x, -1.0)
        if (.not. (ieee_class(neg_b) == ieee_negative_normal)) error stop
    end function
end module
