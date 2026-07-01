program open_newunit
  use iso_fortran_env
  implicit none

  integer :: io_lun

  open (newunit=io_lun, file='open_newunit.dat', status='replace', form='formatted')
  print *, "PASS"

! Test for the first few conditions specified by F2023 12.5.6.13:
  if (io_lun >= -1) error stop 'newunit >= -1'
  if (io_lun == ERROR_UNIT) error stop 'newunit == ERROR_UNIT'
  if (io_lun == INPUT_UNIT) error stop 'newunit == INPUT_UNIT'
  if (io_lun == OUTPUT_UNIT) error stop 'newunit == OUTPUT_UNIT'

  close (io_lun, status='delete')

end program
