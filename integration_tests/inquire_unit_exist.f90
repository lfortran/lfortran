program inquire_exist2
  implicit none

! Test inquire (...exist=...) per Section 12.10.2.11 of F2023

  logical :: exist

  integer, parameter :: lun = 40
  character(*), parameter :: file = 'fort.40'

! Make sure file and unit don't exist and are not pre-connected.

  inquire (file=file, exist=exist)
  if (exist) then
    open (lun, file=file)
    close (lun, status='delete')
  end if

  inquire (unit=lun, exist=exist)
  if (exist) then
    open (lun, file=file)
    close (lun, status='delete')
  end if

! No file

  print *, 'No file by that name (should be False):'

  exist = .true.
  inquire (file=file, exist=exist)
  print *, 'file= inquiry returned: ', exist, pf (.not. exist)

! No preconnection

  print *
  print *, 'No preconnection?  Gfortran considers the unit # as existing, lfortran does not:'

  exist = .true.
  inquire (unit=lun, exist=exist)
  print *, 'unit= inquiry returned: ', exist, pf (exist)  ! By gfortran definition

contains

  elemental function pf (l)
    character(6) :: pf
    logical, intent(in) :: l

    pf = merge (': pass', ': FAIL', l)

  end function

end program