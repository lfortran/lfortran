program read_implied_do
  implicit none
  integer :: i, a(5)
  read(*,*) (a(i), i=1,5)
end program
