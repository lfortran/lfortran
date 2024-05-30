program intrinsics_238
    implicit none
  
    integer :: i
    integer :: res(5)
    res = [ (maskl(i), i=1, 5) ]
    do i = 1, 5
      print*, res(i)
      if (maskl(i) /= res(i) ) error stop
    end do
  
  end program