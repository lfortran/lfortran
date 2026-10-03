program procedure_pointer_21
  implicit none
  abstract interface
    subroutine callback_i(x) bind(C)
      character(len=:), allocatable, intent(out) :: x
    end subroutine
  end interface
  procedure(callback_i), pointer :: cb
  character(len=:), allocatable :: x

  cb => my_sub
  call cb(x)
  if (.not. allocated(x)) error stop "not allocated"
  if (x /= "hello") error stop "wrong value"
  print *, x
contains
  subroutine my_sub(x) bind(C)
    character(len=:), allocatable, intent(out) :: x
    x = "hello"
  end subroutine
end program
