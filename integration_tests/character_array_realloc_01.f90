module character_array_realloc_01_mod
  implicit none
contains
  subroutine populate(vec, s1, s2)
    character(len=*), allocatable, intent(out) :: vec(:)
    character(len=*), intent(in) :: s1, s2
    allocate(vec(2))
    vec(1) = s1
    vec(2) = s2
  end subroutine
end module

program character_array_realloc_01
  use character_array_realloc_01_mod
  implicit none
  character(len=32), allocatable :: arr(:)
  integer :: i

  do i = 1, 3
    call populate(arr, "hello", "world")
    if (.not. allocated(arr)) error stop 1
    if (len(arr) /= 32) error stop 2
    if (trim(arr(1)) /= "hello") error stop 3
    if (trim(arr(2)) /= "world") error stop 4
  end do
end program
