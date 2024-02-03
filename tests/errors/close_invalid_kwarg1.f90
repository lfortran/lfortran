PROGRAM close_invalid_kwarg1
    ! "end" is an invalid keyword argument for "CLOSE"
    ! it is ok to raise "end" being invalid before "unit" being absent
    CLOSE(end=200)
END PROGRAM
