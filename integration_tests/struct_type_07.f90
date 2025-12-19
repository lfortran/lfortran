module struct_type_07_m
  implicit none
  type :: t
     real :: a
  end type t
contains
  function f() result(res)
    type(t) :: res(2,2)
    res%a = reshape([1.0,2.0,3.0,4.0],[2,2])
  end function f
end module struct_type_07_m

program struct_type_07
  use struct_type_07_m
  implicit none
  type(t) :: result(2,2)
  
  result = f()
  
  if (result(1,1)%a /= 1.0) stop 1
  if (result(2,1)%a /= 2.0) stop 2
  if (result(1,2)%a /= 3.0) stop 3
  if (result(2,2)%a /= 4.0) stop 4
  
  print *, "Test passed!"
end program struct_type_07
