! Testing a function is never called twice as a side effect of creating temporary strings (return slot)
program functions_48
    character(:), allocatable :: ss1

    ss1 = ff(f2(), 10)

    print *, len(ss1)
    if(len(ss1) /= 20) error stop 
  
    contains 
    function ff(x, y) result(str)
      integer :: x
      integer :: y
      character(x + y) :: str
    end function
  
    function f2() result(num)
      integer :: num
      integer, save :: called = 0
      num =  10

      print *, "called"
      
      if(called /= 0) error stop
      
      called = called + 1
    end function
end program 