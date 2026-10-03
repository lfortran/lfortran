program modules_67
  use modules_67_consumer, only : test_pairs
  implicit none
  if (test_pairs()) then
    print *, "PASS"
  else
    error stop
  end if
end program
