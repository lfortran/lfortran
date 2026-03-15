program iachar_achar_01
    implicit none
    if (iachar(achar(0)) /= 0) error stop
    if (iachar(achar(65)) /= 65) error stop
    if (achar(65) /= 'A') error stop
end program
