PROGRAM rewind_invalid_kwarg1
    ! "end" is an invalid keyword argument for "REWIND"
    REWIND(end="world")
END PROGRAM
