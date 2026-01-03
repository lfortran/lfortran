program inquire_03
    integer :: unit = 2
    character(12) :: filename, specifier_r, specifier_w, specifier_rw
    filename = "dummy.txt"

    open(unit, file=filename, action='write')
    inquire(unit, write=specifier_w, read=specifier_r, readwrite=specifier_rw)
    print *, specifier_r, specifier_w, specifier_rw
    if (specifier_r /= "NO" .or. specifier_w /= "YES ".or. specifier_rw /= "NO") error stop

    close(unit)

    open(unit, file=filename, action='read')
    inquire(unit, write=specifier_w, read=specifier_r, readwrite=specifier_rw)
    print *, specifier_r, specifier_w, specifier_rw
    if (specifier_r /= "YES" .or. specifier_w /= "NO  " .or. specifier_rw /= "NO") error stop
    close(unit)

    open(unit, file=filename, action='readwrite')
    inquire(unit, write=specifier_w, read=specifier_r, readwrite=specifier_rw)
    print *, specifier_r, specifier_w, specifier_rw
    if (specifier_r /= "YES" .or. specifier_w /= "YES " .or. specifier_rw /= "YES") error stop
    close(unit)

    open(unit, file=filename)
    inquire(unit, write=specifier_w, read=specifier_r, readwrite=specifier_rw)
    print *, specifier_r, specifier_w, specifier_rw
    if (specifier_r /= "YES" .or. specifier_w /= "YES " .or. specifier_rw /= "YES") error stop
    close(unit)

end program inquire_03
