PROGRAM close_invalid_kwarg1
    ! the file "numbers" doesn't need to exist for the test-case
    ! "end" is an invalid keyword argument for "CLOSE"
    ! is it ok to raise "end" being invalid before "unit" being absent?
    CLOSE(end=200)
END PROGRAM
