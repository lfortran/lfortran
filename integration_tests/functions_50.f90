program functions_50
  integer :: n
  character(:), ALLOCATABLE :: x

  n = 5
  x = sayHi()

  if (x /= "Hello") error stop

  print *, x

  contains

  character(n) function sayHi() result (res)
      res = "Hello"
  end function
end program  functions_50
