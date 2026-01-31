program class_character
  implicit none
  class(*), allocatable :: x

  allocate(character(len=11) :: x)
  x = "test passed"

  select type (x)
  type is (character(*))
     print *, x
  class default
     error stop "wrong output"
  end select
end program class_character
