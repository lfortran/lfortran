module allocate_53_mod
  implicit none
  type :: list_item
    character(:), allocatable :: key
  end type
contains
  function copy_item(item) result(copy)
    type(list_item), intent(in) :: item
    type(list_item), pointer :: copy
    allocate(copy, source=item)
  end function
end module

program allocate_53
  use allocate_53_mod
  implicit none
  type(list_item), pointer :: original, copy
  allocate(original)
  original%key = "hello"
  copy => copy_item(original)
  deallocate(original)
  if (copy%key /= "hello") error stop
  deallocate(copy)
  print *, "PASS"
end program
