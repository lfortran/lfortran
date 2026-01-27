program string_95
  character(2), allocatable :: str

  str = f1(5)
  print *, len(str), str
  if (len(str) /= 2) error stop
  if (str /= "ab") error stop

  contains 
  function f1(n) result(res)
    integer :: n
    character(n), allocatable :: res
    allocate(res)
    res = "abcde"
    print *, len(res), res
    if (len(res) /= 5) error stop
    if (res /= "abcde") error stop
  end function
end program string_95