program do_loop_06
    integer :: i
    integer, allocatable :: arr(:)
    allocate(arr(10)) 
    arr = [1,2,3,4,5,6,7,8,9,10]
    do i =1,2
      print *, "dummy do loop" 
      exit
    end do
    print *, arr
    if (any(arr /= [1,2,3,4,5,6,7,8,9,10])) error stop
  end program 