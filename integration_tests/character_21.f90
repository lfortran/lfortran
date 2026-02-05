program character_21
  implicit none
  print *, test_func(2)
  if (test_func(2) /= "119") error stop
  print *, test_func(4)
  if (test_func(4) /= "12345") error stop

contains
  function test_func(d) result(res)
    integer, intent(in) :: d
    character(11) :: res
    character(1) claterdigits*(d+1)
    
    if (d == 2) then
      claterdigits = "119"
    else if (d == 4) then
      claterdigits = "12345"
    end if
    
    res = claterdigits
  end function test_func
end program character_21
