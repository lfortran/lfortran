program flush_05
  implicit none
  integer :: ios
  character(len=256) :: msg

  flush(unit=10, iostat=ios, iomsg=msg)

  print *, 'ios =', ios
  if (ios == 0) error stop
  if (len_trim(msg) == 0) error stop
end program flush_05
