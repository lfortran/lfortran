program struct_type_04
  implicit none

  type :: stone
      integer :: value = 2
  end type stone

  type(stone),dimension(:),allocatable :: s
  allocate(s(2))

  print* , s%value
  print* , sum(s%value) 
  if (sum(s%value) /= 4) error stop
end program