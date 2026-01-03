module a_arrays_77
  contains 
  function f2() result(res)
    integer(8), allocatable :: res(:)
    allocate(res(10))
    res = [1,2,3,4,5,6,7,8,9,10]
  end function f2
end module a_arrays_77

program arrays_77
  use a_arrays_77
  integer :: i
  integer :: arr(10)
  integer,allocatable :: arr_res(:)
  do i = 1 , 10
    arr(i) = i + 100
  end do

  allocate(arr_res(11))
  arr_res = [arr(f2()),111] ! Error here
  print *, arr_res

  do i = 1 , 11
    if (arr_res(i) /= i + 100) error stop
  end do

end program