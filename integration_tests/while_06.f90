program while_06
    integer :: x = 0
    integer, parameter :: skip_tokens(*) = [0,1,2]
    integer :: hit = 0
    do while(any(x == skip_tokens)) 
      print *, "Hi"
      x = 16
      hit = hit + 1
    end do
    
    if(hit /= 1) error stop
end program