program integer_kind_16_boz_nml
    implicit none
    integer(16) :: x
    integer :: unit, ios, found
    character(len=80) :: s
    character(len=240) :: line
    namelist /nml/ x
    x = 255
    write(s, '(b0)') x
    if (trim(s) /= '11111111') error stop 1
    write(s, '(o0)') x
    if (trim(s) /= '377') error stop 2
    write(s, '(z0)') x
    if (trim(s) /= 'FF') error stop 3
    x = -1
    write(s, '(z0)') x
    if (trim(s) /= 'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF') error stop 4
    x = huge(x)
    open(newunit=unit, status='scratch')
    write(unit, nml=nml)
    rewind(unit)
    found = 0
    do
        read(unit, '(a)', iostat=ios) line
        if (ios /= 0) exit
        if (index(line, '170141183460469231731687303715884105727') /= 0) found = 1
    end do
    if (found /= 1) error stop 5
    rewind(unit)
    x = 0
    read(unit, nml=nml)
    if (x /= huge(0_16)) error stop 6
    close(unit)
end program
