program string_27
    CHARACTER(100), DIMENSION(100) :: calibration_document
    INTEGER ::len_line, char_value
    calibration_document(3) = 'abcdefg'
    len_line = 1
    print *, calibration_document(3)(len_line:len_line+4)
    if (calibration_document(3)(len_line:len_line+4) /= 'abcde') error stop
end program
