program write_implied_do_1
  implicit none
  integer :: unit, medium, nmedia
  integer :: n(2)
  character(len=8) :: material(2)

  nmedia = 2
  n = [1, 2]
  material = ['water   ', 'bottom  ']
  unit = 20

  open(unit=unit, file='out.dat', access='direct', recl=128, form='unformatted', status='replace')
  write(unit, rec=1) (n(medium), material(medium), medium = 1, nmedia)
  close(unit)
end program
