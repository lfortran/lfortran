program runtime_input_records
    implicit none
    integer :: u, ios, a(3), i, j
    complex :: z
    character(4) :: text
    character(24) :: record
    namelist /values/ i

    open(newunit=u, file='runtime_input_records.dat', status='replace')
    write(u, '(a)') '1 2 3'
    write(u, '(a)') '(4,5)'
    write(u, '(a)') ''
    write(u, '(a)') 'abcd'
    write(u, '(a)') '21'
    write(u, '(a)') '22'
    write(u, '(a)') '&values i=57 /'
    rewind(u)
    read(u, *, iostat=ios) a
    if (ios /= 0 .or. any(a /= [1,2,3])) error stop
    read(u, *, iostat=ios) z
    if (ios /= 0 .or. abs(z - (4.,5.)) > 1e-6) error stop
    read(u, '(a4)', iostat=ios) text
    if (ios /= 0 .or. text /= '    ') error stop
    read(u, '(a2)', advance='no', iostat=ios) text(:2)
    if (ios /= 0 .or. text(:2) /= 'ab') error stop
    read(u, '(a2)', iostat=ios) text(3:)
    if (ios /= 0 .or. text /= 'abcd') error stop
    read(u, '(i2)', iostat=ios) i,j
    if (ios /= 0 .or. i /= 21 .or. j /= 22) error stop
    read(u, nml=values, iostat=ios)
    if (ios /= 0 .or. i /= 57) error stop
    read(u, *, iostat=ios) i
    if (ios >= 0) error stop
    close(u, status='delete')

    record = '71 72 73'
    read(record, *, iostat=ios) a
    if (ios /= 0 .or. any(a /= [71,72,73])) error stop
    record = '&values i=99 /'
    read(record, nml=values, iostat=ios)
    if (ios /= 0 .or. i /= 99) error stop
end program
