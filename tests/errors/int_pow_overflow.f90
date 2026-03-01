! expect_error
program test_int_pow_overflow
    integer(8) :: x
    x = 2_8**63
end program
