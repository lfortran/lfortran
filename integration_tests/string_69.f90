program string_69
  implicit none
  character(len=7) :: value
  character(len=:), allocatable :: keywords(:)
  integer :: ii
   ii = 7
  value = "version"
  keywords = [character(len=ii) :: value]
  if (len(keywords) /= 7) error stop
  if (keywords(1) /= "version") error stop
  value = "usage"
  keywords = [character(len=ii) :: keywords, value]
  if (keywords(2) /= "usage") error stop
end program