program array_76
  integer :: i
  integer :: arr(10)
  integer :: arr_indecies(10)
  integer,allocatable :: arr_res(:)
  allocate(arr_res(11))

  do i =1,10
    arr(i) = i
    arr_indecies(i) = i
  end do

  arr_res = [arr(arr_indecies),11]
  print *, arr_res

  do i = 1 , 11
    if (arr_res(i) /= i) error stop
  end do
end program
