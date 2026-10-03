program read_96
  implicit none

  integer :: unit, medium, nmedia
  integer :: n(2)
  character(len=8) :: material(2)

  nmedia = 2
  unit = 20

  open(unit=unit, file='in.dat', access='direct', recl=128, form='unformatted')
  write(unit, rec=1) 1, 'water   ', 2, 'bottom  '
  read(unit, rec=1) (n(medium), material(medium), medium = 1, nmedia)

  close(unit)

  if (n(1) /= 1) error stop "first value incorrect"
  if (material(1) /= 'water   ') error stop "first string incorrect"
  if (n(2) /= 2) error stop "second value incorrect"
  if (material(2) /= 'bottom  ') error stop "second string incorrect"

end program read_96