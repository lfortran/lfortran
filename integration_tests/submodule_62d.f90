! Also the only user of ieee_arithmetic.
submodule (submodule_62_m1) submodule_62_s
    use, intrinsic :: ieee_arithmetic, only: ieee_class, ieee_value, &
        ieee_positive_normal, ieee_quiet_nan, ieee_is_nan, operator(==)
    use submodule_62_r
    use submodule_62_q
    implicit none
contains
    module procedure s_step
        r = r_bump()
        q = q_bump()
    end procedure

    module procedure s_class_ok
        real :: x
        x = 1.5
        ok = ieee_class(x) == ieee_positive_normal
        x = ieee_value(x, ieee_quiet_nan)
        ok = ok .and. ieee_is_nan(x)
    end procedure
end submodule
