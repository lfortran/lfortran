program implied_do_loops44
  implicit none
  character(len=80), allocatable :: manual(:)
  character(len=80) :: buf
  integer :: i

  manual = [character(len=5) :: 'one', 'two', 'three']

  write(buf,'(3(g0))') ( [character(len=80/3) :: manual(i)], i=1, size(manual) )
  
  if (index(buf, "one") == 0 .or. index(buf, "two") == 0 .or. index(buf, "three") == 0) then
      print *, "Expected one, two, three, got ", trim(buf)
      error stop 1
  end if
  print *, "OK"
end program implied_do_loops44
