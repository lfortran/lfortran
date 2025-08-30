! Refer to ./doc/src/string_allocation.md

program string_allocation_01
    implicit none
    character(:), allocatable :: a
    integer :: i
    i  = -10
    allocate(character(i) :: a)
  end program