program test_prog
    use sep_comp_ext_sym_use_m1
    use sep_comp_ext_sym_use_m2
    implicit none

    if (foo() /= sep_comp_ext_sym_expected) error stop
end program
