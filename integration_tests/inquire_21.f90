program inquire_21
    ! Exercises the INQUIRE specifiers that the ASR -> Fortran printer
    ! (--show-fortran) emits, while staying a runnable program.
    implicit none
    integer :: u, v, number, recl, nextrec, iol, ios, size4, pos4
    integer(8) :: size8, pos8
    logical :: exist, opened, named, pending
    character(len=64) :: name, iomsg
    character(len=32) :: access, sequential, direct, form, formatted, &
        unformatted, blank, position, action, read, write, readwrite, &
        delim, pad, decimal, sign, encoding, round, stream, asynchronous
    integer :: i4
    real :: r4

    open(newunit=u, file='inquire_21_data.txt', status='replace', &
        form='formatted', access='sequential', action='readwrite', &
        blank='null', delim='quote', pad='yes')
    write(u, '(a)') 'hello'

    inquire(unit=u, exist=exist, opened=opened, number=number, &
        named=named, name=name, access=access, sequential=sequential, &
        direct=direct, form=form, formatted=formatted, &
        unformatted=unformatted, recl=recl, nextrec=nextrec, blank=blank, &
        position=position, action=action, read=read, write=write, &
        readwrite=readwrite, delim=delim, pad=pad, decimal=decimal, &
        sign=sign, encoding=encoding, round=round, pending=pending, &
        asynchronous=asynchronous, iostat=ios, iomsg=iomsg)
    if (.not. exist) error stop 1
    if (.not. opened) error stop 2
    if (number /= u) error stop 3
    if (.not. named) error stop 4
    if (trim(name) /= 'inquire_21_data.txt') error stop 5
    if (trim(access) /= 'SEQUENTIAL') error stop 6
    if (trim(sequential) /= 'YES') error stop 7
    if (trim(direct) /= 'NO') error stop 8
    if (trim(form) /= 'FORMATTED') error stop 9
    if (trim(formatted) /= 'YES') error stop 10
    if (trim(unformatted) /= 'NO') error stop 11
    if (trim(blank) /= 'NULL') error stop 12
    if (trim(action) /= 'READWRITE') error stop 14
    if (trim(read) /= 'YES') error stop 15
    if (trim(write) /= 'YES') error stop 16
    if (trim(readwrite) /= 'YES') error stop 17
    if (trim(delim) /= 'QUOTE') error stop 18
    if (trim(pad) /= 'YES') error stop 19
    if (trim(decimal) /= 'POINT') error stop 20
    if (trim(encoding) /= 'DEFAULT') error stop 21
    if (trim(round) /= 'PROCESSOR_DEFINED') error stop 22
    if (pending) error stop 23
    if (trim(asynchronous) /= 'NO') error stop 24
    if (ios /= 0) error stop 25
    close(u, status='delete')

    ! INQUIRE by file name, once the file is gone again.
    inquire(file='inquire_21_data.txt', exist=exist, opened=opened, &
        number=number, named=named, name=name, access=access, &
        sequential=sequential, direct=direct, form=form, &
        formatted=formatted, unformatted=unformatted, recl=recl, &
        blank=blank, action=action, read=read, write=write, &
        readwrite=readwrite, delim=delim, pad=pad, decimal=decimal, &
        sign=sign, encoding=encoding, round=round, iostat=ios, iomsg=iomsg)
    if (exist) error stop 26
    if (opened) error stop 27

    ! SIZE=, POS= and STREAM= are queried on a stream unit.
    open(newunit=v, file='inquire_21_stream.txt', status='replace', &
        access='stream', form='formatted', action='readwrite')
    write(v, '(a)') 'abcdef'
    inquire(unit=v, size=size8, pos=pos8, stream=stream)
    if (size8 /= 7_8) error stop 28
    if (pos8 /= 8_8) error stop 29
    if (trim(stream) /= 'YES') error stop 30
    inquire(unit=v, size=size4, pos=pos4)
    if (size4 /= 7) error stop 31
    if (pos4 /= 8) error stop 32
    close(v, status='delete')

    ! IOLENGTH= with positional output items.
    inquire(iolength=iol) i4
    if (iol /= 4) error stop 33
    inquire(iolength=iol) i4, r4
    if (iol /= 8) error stop 34

    print *, 'ok'
end program
