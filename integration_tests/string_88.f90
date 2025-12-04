! This program checks Len Function 
! On allocation of String Arrays with size = 0
program string_88
  implicit none
  character(len=:), allocatable :: list1(:), list2(:)
  character(len=23), pointer :: list3(:) !Using pointer allocation
  list1 = [character(len=23):: ]
  allocate(character(len=23) :: list2(0))
  allocate(list3(0))
  print *,len(list1), len(list2), len(list3)
  if (len(list1) /= 23) error stop
  if (len(list2) /= 23) error stop
  if (len(list3) /= 23) error stop
end program string_88