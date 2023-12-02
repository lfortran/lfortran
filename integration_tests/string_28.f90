program string_28
    CHARACTER(100), DIMENSION(100) :: calibration_document
    INTEGER ::len_line, char_value
    calibration_document(3) = 'abc'
    len_line = 1
    char_value = IACHAR(calibration_document(3)(len_line:len_line))
    print *, char_value
    if (char_value /= 97) error stop
end program
