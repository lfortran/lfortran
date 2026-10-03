program a
  implicit none
  ! Declare an unlimited polymorphic pointer
  class(*), pointer :: ptr
  character(5), target :: t
  t = 'abcde'
  ptr => t
  select type (p => ptr)
    type is (character(*))
      print *, p
      if (p /= 'abcde') error stop
      p = 'vwxyz'
    class default
      print *, "Unknown"
      error stop
  end select
  print *, t
  if (t /= 'vwxyz') error stop
end program a