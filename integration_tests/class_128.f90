program class_128
  implicit none
  type :: t
    class(*), allocatable :: value
  end type
  type(t) :: x

  call set_val(x, 'foo')
  select type (v => x%value)
  type is (character(*))
    if (v /= 'foo') error stop
  class default
    error stop
  end select

  call set_val(x, 'hello')
  select type (v => x%value)
  type is (character(*))
    if (v /= 'hello') error stop
  class default
    error stop
  end select

  call set_val(x, 42)
  select type (v => x%value)
  type is (integer)
    if (v /= 42) error stop
  class default
    error stop
  end select

  print *, "PASS"
contains
  subroutine set_val(this, value)
    type(t), intent(out) :: this
    class(*), intent(in) :: value
    this%value = value
  end subroutine
end program
