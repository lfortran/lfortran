program inquire_03
    integer :: unit = 2
    character(12) :: filename, specifier
    filename = "dummy.txt"
    open(unit, file=filename, action='readwrite')
    inquire(unit, write=specifier)
    if (specifier(1:1) /= 'Y' .and. specifier(1:1) /= 'y') error stop
end program inquire_03
