program allocatable_oob_01
  implicit none
  character(len=10), allocatable :: recvBuffer(:)
  allocate(recvBuffer(1))
  recvBuffer = ''
  print *, trim(recvBuffer(11))
end program allocatable_oob_01
