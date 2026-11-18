program write_implied_do_1
  implicit none
  integer :: unit, medium, nmedia
  integer :: n(2), n2(2)
  character(len=8) :: material(2), material2(2)

  nmedia = 2
  n = [1, 2]
  material = ['water   ', 'bottom  ']
  unit = 20

  open(unit=unit, file='write_implied_do_1.dat', access='direct', recl=128, &
      form='unformatted', status='replace')
  write(unit, rec=1) (n(medium), material(medium), medium = 1, nmedia)
  read(unit, rec=1) (n2(medium), material2(medium), medium = 1, nmedia)
  close(unit, status='delete')
  if (any(n2 /= n)) error stop "wrong integers"
  if (any(material2 /= material)) error stop "wrong strings"
end program
