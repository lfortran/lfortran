module derived_types_138_mod
  implicit none

  type :: my_type
    integer :: x = 0
    integer, allocatable :: data(:)
  contains
    final :: cleanup
  end type

contains
  subroutine cleanup(this)
    type(my_type), intent(inout) :: this
    if (allocated(this%data)) deallocate(this%data)
    this%x = 0
  end subroutine
end module

program derived_types_138
  use derived_types_138_mod
  implicit none
  type(my_type), allocatable :: a, b
  class(my_type), allocatable :: c

  allocate(a)
  a%x = 10
  allocate(a%data(3))
  a%data = [1, 2, 3]

  allocate(b)
  b%x = 20

  b = a
  if (b%x /= 10) error stop
  if (b%data(2) /= 2) error stop

  allocate(my_type :: c)
  c%x = 42
  deallocate(c)

  deallocate(a)
  deallocate(b)
  allocate(a)
  a%x = 50
  allocate(b)
  b%x = 99
  call move_alloc(a, b)
  if (b%x /= 50) error stop

  print *, "ok"
end program
