! expect_error
program test_int_pow_negative_exponent
    integer :: x
    x = 2**(-3)
end program
