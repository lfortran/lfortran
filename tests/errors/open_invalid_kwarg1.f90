PROGRAM open_invalid_kwarg1
    ! the file "numbers" doesn't need to exist for the test-case
    ! "hello" is an invalid keyword argument for "OPEN"
    OPEN(file="numbers", hello="world")
END PROGRAM
