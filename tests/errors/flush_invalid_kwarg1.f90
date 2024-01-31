PROGRAM flush_invalid_kwarg1
    ! "start" is an invalid keyword argument for "REWIND"
    FLUSH(unit=10, start=100)
END PROGRAM
