program character_explicit
  character(10) :: buf(2)

  if (any(count_vals(buf) /= 10)) error stop
  print *, "Test passed"
contains

  function count_vals(items) result(res)
    character(len=*), dimension(10) :: items
    integer :: res(size(items))

    res = 10
  end function count_vals

end program character_explicit
