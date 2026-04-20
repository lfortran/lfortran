program allocate_source_ptr_01
  implicit none
  type :: t
    integer :: x
  end type
  type(t), pointer :: src, dest
  allocate(src)
  src%x = 1
  allocate(dest, source=src)
  if (dest%x /= 1) error stop "dest%x should be 1"
  ! Verify deep copy, not aliasing
  src%x = 2
  if (dest%x /= 1) error stop "shallow copy detected"
  print *, "PASS"
  deallocate(src)
  deallocate(dest)
end program
