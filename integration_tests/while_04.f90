! Check handling while's test expression when a temporary is needed
program while_04
    integer :: flag = 1
    integer :: hit = 0
    
    do while(foo(flag) == "Hello")
      print *, "hi"
      flag = -1
      hit = hit + 1
    end do
  
    if(hit /= 1) error stop
  
    contains
    function foo(x) result (str)
      integer :: x
      character(:),allocatable :: str
      if(x == 1) then
        str = "Hello"
      else 
        str = "bla-bla"
      end if
    end function
  
  end program 