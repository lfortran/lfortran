program pointer_null_01
  call sink(null())

contains

  subroutine sink(x)
    integer, pointer :: x(..)
    if (associated(x)) error stop
  end subroutine sink

end program 
