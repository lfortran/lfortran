program select_type_16
  class(*), allocatable :: s
  call ss("hello")

  contains 
  subroutine ss(x)
    class(*) :: x
    character(:),allocatable :: buffer
    allocate(character(len=100) :: buffer)

    select type(x)
    type is (character(len=*))
      write(buffer, "(A,A)") "string:", x
    class default 
      error stop "unknown type"
    end select

    print *, buffer
    if(trim(buffer) /= "string:hello") error stop
  end subroutine
end program