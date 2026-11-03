! A namelist write to a scalar character variable writes the whole group into
! its single record; a record that is too short is an end-of-record condition.
subroutine namelist_41_sub()
    real :: x
    integer :: n(3)
    logical :: l
    namelist /namdyn/ x, n, l
    character(len=200) :: buf
    character(len=10) :: small
    integer :: ios
    x = 1.5
    n = [1, 2, 3]
    l = .true.
    write(buf, nml=namdyn)
    print *, trim(buf)
    if (index(buf, "&NAMDYN") == 0) error stop
    if (index(buf, "/") == 0) error stop
    x = 0
    n = 0
    l = .false.
    read(buf, nml=namdyn)
    if (abs(x - 1.5) > 1e-6) error stop
    if (any(n /= [1, 2, 3])) error stop
    if (.not. l) error stop

    buf = repeat("z", 200)
    write(buf(11:150), nml=namdyn)
    if (buf(1:10) /= repeat("z", 10)) error stop
    if (buf(151:200) /= repeat("z", 50)) error stop
    x = 0
    n = 0
    l = .false.
    read(buf(11:150), nml=namdyn)
    if (abs(x - 1.5) > 1e-6) error stop
    if (any(n /= [1, 2, 3])) error stop
    if (.not. l) error stop

    write(small, nml=namdyn, iostat=ios)
    if (.not. is_iostat_eor(ios)) error stop
end subroutine

program namelist_41
    call namelist_41_sub()
end program
