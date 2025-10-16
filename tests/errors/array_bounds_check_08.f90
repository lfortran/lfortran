program array_bounds_check_08
  integer, allocatable :: arr(:)
  allocate(arr(5))
  arr = ff()
  print *, size(arr)

  contains
    function ff() result(res)
        integer :: res(100000)
        res(100000) = 9999
    end function
end program
