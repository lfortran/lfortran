program read_nonadvance_size_01
    implicit none
    integer :: u, a, b, sz

    open(newunit=u, status='scratch', action='readwrite')
    write(u, '(a)') '1 2'
    rewind(u)
    read(u, '(i1,1x,i1)', advance='no', size=sz) a, b
    if (a /= 1) error stop 'wrong first value with x descriptor'
    if (b /= 2) error stop 'wrong second value with x descriptor'
    if (sz /= 3) error stop 'wrong size with x descriptor'
    close(u)

    open(newunit=u, status='scratch', action='readwrite')
    write(u, '(a)') '12 34 56xyz'
    rewind(u)
    read(u, '(i2,i2,i2)', advance='no', size=sz) a, b
    if (a /= 12) error stop 'wrong first value with wider fields'
    if (b /= 3) error stop 'wrong second value with wider fields'
    if (sz /= 4) error stop 'wrong size with wider fields'
    close(u)
end program read_nonadvance_size_01
