module nested_vars_11_mod
  implicit none
contains
  subroutine host(path)
    character(len=:), allocatable, intent(out) :: path
    call inner()
  contains
    subroutine inner()
      if (allocated(path)) error stop 1
      path = "abc"
    end subroutine
  end subroutine
end module

program nested_vars_11
  use nested_vars_11_mod
  implicit none
  character(len=:), allocatable :: p
  integer :: i
  do i = 1, 3
    call host(p)
    if (.not. allocated(p)) error stop 2
    if (len(p) /= 3) error stop 3
    if (p /= "abc") error stop 4
  end do
end program
