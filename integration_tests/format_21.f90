program format_21
    !> add tests to ensure that colon edit descriptor is supported
    print "(I2, :, A)", 42
    print "(I2, :, A)", 42, " is the answer"
    print "(I3, :, I10)", 42, 43
    print "(A, I2, :, A)", "The touch is ", 42, " degrees"
    print "(A, I2, :, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A)", "The touch is ", 42
end program format_21
