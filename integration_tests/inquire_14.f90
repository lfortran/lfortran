#define TEST_F2003
#define TEST_F2003ASYNC
#define TEST_F2018

program inquire_14
  implicit none

! Test various INQUIRE return values

! Most of these tests are based on Table C.1 in Section C.6.5 of F95.
! F2003, F2018 and F2023 can optionally be enabled.

  character(20) :: access, action, blank, delim, direct, form, formatted
  character(20) :: name, pad, position, read, readwrite, sequential
  character(20) :: unformatted, write

  logical :: exist, named, opened
  integer :: iostat, nextrec, number, recl

#ifdef TEST_F2003
  character(20) :: decimal, encoding, round, signc, stream
  integer :: pos
#endif
#ifdef TEST_F2003ASYNC
  character(16) :: async
  integer :: id
  logical :: pending
#endif
#ifdef TEST_F2023
  character(16) :: leading_zero
#endif

  integer, parameter :: lun = 42
  character(*), parameter :: lfn = 'inquire_14_file.txt'
! Make sure there is no file with the name we will be using
  open (lun, file=lfn, status='old', iostat=iostat)
  if (iostat == 0) then
    close (lun, status='delete')
  end if

! File not connected yet
  call init_vars ()
  inquire (file=lfn, access=access, action=action, blank=blank, delim=delim,  &
      direct=direct, form=form, formatted=formatted, pad=pad, position=position,  &
      name=name, named=named, number=number, opened=opened,  &
      read=read, readwrite=readwrite, sequential=sequential, unformatted=unformatted,  &
      write=write, iostat=iostat)
! In two cases, namely name= and pad=, the F95 draft is wrong, and F2003-onwards
! are correct.
  call pf (access == 'UNDEFINED')
  call pf (action == 'UNDEFINED')
  call pf (blank  == 'UNDEFINED')
  call pf (delim  == 'UNDEFINED')
  call pf (direct == 'UNKNOWN')
  call pf (.not. exist)
  call pf (form   == 'UNDEFINED')
  call pf (formatted == 'UNKNOWN')
  call pf (iostat == 0)
  call pf (name   == lfn)! (F95 says 'not the same as FILE= value')
  call pf (named)
  call pf (number == -1)
  call pf (.not. opened)
  call pf (pad    == 'UNDEFINED')! F95 says 'YES' which is wrong
  call pf (position == 'UNDEFINED')
  call pf (read   == 'UNKNOWN')
  call pf (readwrite == 'UNKNOWN')
  call pf (sequential == 'UNKNOWN')
  call pf (unformatted == 'UNKNOWN')
  call pf (write   == 'UNKNOWN')
#ifdef TEST_F2003
  inquire (file=lfn, decimal=decimal, encoding=encoding, pos=pos,  &
      round=round, sign=signc, stream=stream)
  call pf (decimal == 'UNDEFINED')
  call pf (encoding == 'UNKNOWN')
  call pf (round == 'UNDEFINED')
  call pf (signc == 'UNDEFINED')
  call pf (stream == 'UNKNOWN')
#endif
#ifdef TEST_F2003ASYNC
  inquire (file=lfn, asynchronous=async)
  call pf (async == 'UNDEFINED')
#endif
#ifdef TEST_F2018
  inquire (file=lfn, recl=recl)
  call pf (recl   == -1)
#endif
#ifdef TEST_F2023
  inquire (file=lfn, leading_zero=leading_zero)
  call pf (leading_zero == 'UNDEFINED')
#endif

! Unit not connected yet
  call init_vars ()
  inquire (unit=lun, access=access, action=action, blank=blank, delim=delim,  &
      direct=direct, form=form, formatted=formatted, pad=pad, position=position,  &
      name=name, named=named, number=number, opened=opened,  &
      read=read, readwrite=readwrite, sequential=sequential, unformatted=unformatted,  &
      write=write, iostat=iostat)
! In two cases, namely name= and pad=, the F95 draft is wrong, and F2003-onwards
! are correct.
  call pf (access == 'UNDEFINED')
  call pf (action == 'UNDEFINED')
  call pf (blank  == 'UNDEFINED')
  call pf (delim  == 'UNDEFINED')
  call pf (direct == 'UNKNOWN')
  call pf (.not. exist)
  call pf (form   == 'UNDEFINED')
  call pf (formatted == 'UNKNOWN')
  call pf (iostat == 0)
  call pf (name   == 'xxx')! Assume untouched (F95 says 'UNDEFINED')
  call pf (.not. named)
  call pf (number == -1)
  call pf (.not. opened)
  call pf (pad    == 'UNDEFINED')! F95 says 'YES' which is wrong
  call pf (position == 'UNDEFINED')
  call pf (read   == 'UNKNOWN')
  call pf (readwrite == 'UNKNOWN')
  call pf (sequential == 'UNKNOWN')
  call pf (unformatted == 'UNKNOWN')
  call pf (write   == 'UNKNOWN')
#ifdef TEST_F2003
  inquire (unit=lun, decimal=decimal, encoding=encoding, pos=pos,  &
      round=round, sign=signc, stream=stream)
  call pf (decimal == 'UNDEFINED')
  call pf (encoding == 'UNKNOWN')
  call pf (round == 'UNDEFINED')
  call pf (signc == 'UNDEFINED')
  call pf (stream == 'UNKNOWN')
#endif
#ifdef TEST_F2003ASYNC
  inquire (unit=lun, asynchronous=async)
  call pf (async == 'UNDEFINED')
#endif
#ifdef TEST_F2018
  inquire (unit=lun, recl=recl)
  call pf (recl   == -1)
#endif
#ifdef TEST_F2023
  inquire (unit=lun, leading_zero=leading_zero)
  call pf (leading_zero == 'UNDEFINED')
#endif

! Open the file to connect it to the unit - sequential access

#if !defined(__LFORTRAN__)
  open (lun, file=lfn, status='new', access='sequential', form='formatted', recl=80)
#else
  open (lun, file=lfn, status='new', access='sequential', form='formatted')
#endif
  write (lun,*) 'hello world!'
  rewind (lun)

  call init_vars ()
  inquire (file=lfn, access=access, action=action, blank=blank, delim=delim,  &
      direct=direct, form=form, formatted=formatted, pad=pad, position=position,  &
      name=name, named=named, number=number, opened=opened, recl=recl,  &
      read=read, readwrite=readwrite, sequential=sequential, unformatted=unformatted,  &
      write=write, iostat=iostat)
  call pf (access == 'SEQUENTIAL')
  call pf (action == 'READWRITE')
  call pf (blank  == 'NULL')
  call pf (delim  == 'NONE')
  call pf (direct == 'NO')
  call pf (.not. exist)
  call pf (form   == 'FORMATTED')
  call pf (formatted == 'YES')
  call pf (iostat == 0)
  call pf (name   == lfn)
  call pf (named)
  call pf (number == lun)
  call pf (opened)
  call pf (pad    == 'YES')
  call pf (position == 'REWIND')
  call pf (read   == 'YES')
  call pf (readwrite == 'YES')
#if !defined(__LFORTRAN__)
  call pf (recl   == 80)
#endif
  call pf (sequential == 'YES')
  call pf (unformatted == 'NO')
  call pf (write   == 'YES')
#ifdef TEST_F2003
  inquire (file=lfn, decimal=decimal, encoding=encoding, pos=pos,  &
      round=round, sign=signc, stream=stream)
  call pf (decimal == 'POINT')
  call pf (encoding == 'DEFAULT')
  call pf (round == 'PROCESSOR_DEFINED')
  call pf (signc == 'PROCESSOR_DEFINED')
  call pf (stream == 'NO')
#endif

! Unit is connected
  call init_vars ()
  inquire (unit=lun, access=access, action=action, blank=blank, delim=delim,  &
      direct=direct, form=form, formatted=formatted, pad=pad, position=position,  &
      name=name, named=named, number=number, opened=opened, recl=recl,  &
      read=read, readwrite=readwrite, sequential=sequential, unformatted=unformatted,  &
      write=write, iostat=iostat)
  call pf (access == 'SEQUENTIAL')
  call pf (action == 'READWRITE')
  call pf (blank  == 'NULL')
  call pf (delim  == 'NONE')
  call pf (direct == 'NO')
  call pf (.not. exist)
  call pf (form   == 'FORMATTED')
  call pf (formatted == 'YES')
  call pf (iostat == 0)
  call pf (name   == lfn)
  call pf (named)
  call pf (number == lun)
  call pf (opened)
  call pf (pad    == 'YES')
  call pf (position == 'REWIND')
  call pf (read   == 'YES')
  call pf (readwrite == 'YES')
#if !defined(__LFORTRAN__)
  call pf (recl   == 80)
#endif
  call pf (sequential == 'YES')
  call pf (unformatted == 'NO')
  call pf (write   == 'YES')
#ifdef TEST_F2003
  inquire (unit=lun, decimal=decimal, encoding=encoding, pos=pos,  &
      round=round, sign=signc, stream=stream)
  call pf (decimal == 'POINT')
  call pf (encoding == 'DEFAULT')
  call pf (round == 'PROCESSOR_DEFINED')
  call pf (signc == 'PROCESSOR_DEFINED')
  call pf (stream == 'NO')
#endif

  close (lun, status='delete')

#ifdef TEST_F2003
! Open the file to connect it to the unit - sequential stream access

  open (lun, file=lfn, status='new', access='stream', form='unformatted')
  write (lun) 'hello world!'
  rewind (lun)

  call init_vars ()
  inquire (file=lfn, access=access, action=action, blank=blank, delim=delim,  &
      direct=direct, form=form, formatted=formatted, pad=pad, position=position,  &
      name=name, named=named, number=number, opened=opened,  &
      read=read, readwrite=readwrite, sequential=sequential, unformatted=unformatted,  &
      write=write, iostat=iostat)
  call pf (access == 'STREAM')
  call pf (action == 'READWRITE')
  call pf (blank  == 'UNDEFINED')
  call pf (delim  == 'UNDEFINED')
  call pf (direct == 'NO')
  call pf (.not. exist)
  call pf (form   == 'UNFORMATTED')
  call pf (formatted == 'NO')
  call pf (iostat == 0)
  call pf (name   == lfn)
  call pf (named)
  call pf (number == lun)
  call pf (opened)
  call pf (pad    == 'UNDEFINED')
  call pf (position == 'REWIND')
  call pf (read   == 'YES')
  call pf (readwrite == 'YES')
  call pf (sequential == 'NO')! ????
  call pf (unformatted == 'YES')
  call pf (write   == 'YES')
  inquire (file=lfn, decimal=decimal, encoding=encoding, pos=pos,  &
      round=round, sign=signc, stream=stream)
  call pf (decimal == 'UNDEFINED')
  call pf (encoding == 'UNDEFINED')
  call pf (round == 'PROCESSOR_DEFINED')
  call pf (signc == 'PROCESSOR_DEFINED')
  call pf (stream == 'YES')
#ifdef TEST_F2018
  inquire (file=lfn, recl=recl)
  call pf (recl   == -2)
#endif

! Unit is connected
  call init_vars ()
  inquire (unit=lun, access=access, action=action, blank=blank, delim=delim,  &
      direct=direct, form=form, formatted=formatted, pad=pad, position=position,  &
      name=name, named=named, number=number, opened=opened,  &
      read=read, readwrite=readwrite, sequential=sequential, unformatted=unformatted,  &
      write=write, iostat=iostat)
  call pf (access == 'STREAM')
  call pf (action == 'READWRITE')
  call pf (blank  == 'UNDEFINED')
  call pf (delim  == 'UNDEFINED')
  call pf (direct == 'NO')
  call pf (.not. exist)
  call pf (form   == 'UNFORMATTED')
  call pf (formatted == 'NO')
  call pf (iostat == 0)
  call pf (name   == lfn)
  call pf (named)
  call pf (number == lun)
  call pf (opened)
  call pf (pad    == 'UNDEFINED')
  call pf (position == 'REWIND')
  call pf (read   == 'YES')
  call pf (readwrite == 'YES')
  call pf (sequential == 'NO')! ????
  call pf (unformatted == 'YES')
  call pf (write   == 'YES')
  inquire (unit=lun, decimal=decimal, encoding=encoding, pos=pos,  &
      round=round, sign=signc, stream=stream)
  call pf (decimal == 'UNDEFINED')
  call pf (encoding == 'UNDEFINED')
  call pf (round == 'PROCESSOR_DEFINED')
  call pf (signc == 'PROCESSOR_DEFINED')
  call pf (stream == 'YES')
#ifdef TEST_F2018
  inquire (unit=lun, recl=recl)
  call pf (recl   == -2)
#endif

  close (lun, status='delete')
#endif

! Open the file to connect it to the unit - direct access

  open (lun, file=lfn, status='new', access='direct', recl=12, form='formatted')
  write (lun,rec=1, fmt='(a)') 'hello world!'

  call init_vars ()
  inquire (file=lfn, access=access, action=action, blank=blank, delim=delim,  &
      direct=direct, form=form, formatted=formatted, nextrec=nextrec, pad=pad,  &
      name=name, named=named, number=number, opened=opened,  &
      position=position, read=read, readwrite=readwrite, recl=recl,  &
      sequential=sequential, unformatted=unformatted, write=write, iostat=iostat)
  call pf (access == 'DIRECT')
  call pf (action == 'READWRITE')
  call pf (blank  == 'NULL')
  call pf (delim  == 'NONE')
  call pf (direct == 'YES')
  call pf (.not. exist)
  call pf (form   == 'FORMATTED')
  call pf (formatted == 'YES')
  call pf (iostat == 0)
  call pf (name   == lfn)
  call pf (named)
  call pf (nextrec == 2)
  call pf (number == lun)
  call pf (opened)
  call pf (pad    == 'YES')
  call pf (position == 'UNDEFINED')
  call pf (read   == 'YES')
  call pf (readwrite == 'YES')
  call pf (recl   == 12)
  call pf (sequential == 'NO')
  call pf (unformatted == 'NO')
  call pf (write   == 'YES')
#ifdef TEST_F2003
  inquire (file=lfn, decimal=decimal, encoding=encoding, pos=pos,  &
      round=round, sign=signc, stream=stream)
  call pf (decimal == 'POINT')
  call pf (encoding == 'DEFAULT')
  call pf (round == 'PROCESSOR_DEFINED')
  call pf (signc == 'PROCESSOR_DEFINED')
  call pf (stream == 'NO')
#endif

! Unit is connected
  call init_vars ()
  inquire (unit=lun, access=access, action=action, blank=blank, delim=delim,  &
      direct=direct, form=form, formatted=formatted, nextrec=nextrec, pad=pad,  &
      name=name, named=named, number=number, opened=opened,  &
      position=position, read=read, readwrite=readwrite, recl=recl,  &
      sequential=sequential, unformatted=unformatted, write=write, iostat=iostat)
  call pf (access == 'DIRECT')
  call pf (action == 'READWRITE')
  call pf (blank  == 'NULL')
  call pf (delim  == 'NONE')
  call pf (direct == 'YES')
  call pf (.not. exist)
  call pf (form   == 'FORMATTED')
  call pf (formatted == 'YES')
  call pf (iostat == 0)
  call pf (name   == lfn)
  call pf (named)
  call pf (nextrec == 2)
  call pf (number == lun)
  call pf (opened)
  call pf (pad    == 'YES')
  call pf (position == 'UNDEFINED')
  call pf (read   == 'YES')
  call pf (readwrite == 'YES')
  call pf (recl   == 12)
  call pf (sequential == 'NO')
  call pf (unformatted == 'NO')
  call pf (write   == 'YES')
#ifdef TEST_F2003
  inquire (unit=lun, decimal=decimal, encoding=encoding, pos=pos,  &
      round=round, sign=signc, stream=stream)
  call pf (decimal == 'POINT')
  call pf (encoding == 'DEFAULT')
  call pf (round == 'PROCESSOR_DEFINED')
  call pf (signc == 'PROCESSOR_DEFINED')
  call pf (stream == 'NO')
#endif

  close (lun)

contains

  subroutine init_vars ()

    exist = .false.
    named = .false.
    opened = .false.

    access = 'xxx'
    action = 'xxx'
    blank = 'xxx'
    delim = 'xxx'
    direct = 'xxx'
    form = 'xxx'
    formatted = 'xxx'
    iostat = -40
    name = 'xxx'
    nextrec = -41
    number = -42
    pad = 'xxx'
    position = 'xxx'
    read = 'xxx'
    readwrite = 'xxx'
    recl = -43
    sequential = 'xxx'
    unformatted = 'xxx'
    write = 'xxx'

#ifdef TEST_F2003
    decimal = 'xxx'
    encoding = 'xxx'
    round = 'xxx'
    pos = -44
    signc = 'xxx'
    stream = 'xxx'
#endif
#ifdef TEST_F2003ASYNC
    async = 'xxx'
    id = -45
#endif
#ifdef TEST_F2023
    leading_zero = 'xxx'
#endif

  end subroutine
  subroutine pf (iof)
    logical, intent(in) :: iof

    if (.not. iof) then
      error stop 'inquire test assertion failed'
    end if

  end subroutine
end program
