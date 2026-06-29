program do_implied3
  implicit none

  character(60), parameter :: input_data(2) =  &
      ['246801357912345678901234      ', '.10203040506070809010E+0233.33']
  character(60) :: str_data(2)
  real :: rdata(5), r_expected(5)
  integer :: i

  str_data = input_data
  rdata = 0.0
  i = -42
  read (str_data,100) (rdata(i),i=1,size (rdata))
  if (i /= size (rdata) + 1) error stop 1

  r_expected = [246.8, 135.79, 1.234567890E13, 10.20304, 33.33]
  do, i=1, size (rdata)
    if (abs (rdata(i) - r_expected(i)) >= 0.0001) error stop 2
  end do

100   format (2F5.2, F14.0 / E25.20, F5.2)

end program
