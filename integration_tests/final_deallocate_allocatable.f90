module m
  type :: t
    integer :: x
  contains
    final :: destroy
  end type
contains
  subroutine destroy(obj)
    type(t), intent(inout) :: obj
    print *, "FINAL"
  end subroutine
end module

program test
  use m
  type(t), allocatable :: a
  allocate(a)
  deallocate(a)
end program