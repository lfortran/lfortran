program daio6
  implicit none

  character(*), parameter :: fn = 'fort.42'
  integer, parameter :: daunit = 42
  integer :: darl, danr

  integer :: idata

  darl = 5
  open (daunit, file=fn, access='direct', recl=darl, form='unformatted')

  inquire (unit=daunit, nextrec=danr)
  if (danr /= 1) error stop
  inquire (file=fn, NEXTREC=danr)
  if (danr /= 1) error stop

  idata = 42
  write (daunit, rec=1) idata

  inquire (unit=daunit, nextrec=danr)
  if (danr /= 2) error stop
  inquire (file=fn, NEXTREC=danr)
  if (danr /= 2) error stop

! Read a record

  read (daunit, rec=1) idata

  inquire (unit=daunit, nextrec=danr)
  if (danr /= 2) error stop
  inquire (file=fn, nextrec=danr)
  if (danr /= 2) error stop

  close (daunit)

end program
