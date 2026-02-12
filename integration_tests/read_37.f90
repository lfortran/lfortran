program p
  type :: t
     integer :: a
     character(len=10) :: b
  end type t

  type(t) :: x
  character(len=10) :: buf1
  character(len=10) :: buf2

  buf1 = "42"
  buf2 = "hello"

  read(buf1, *) x%a
  read(buf2, *) x%b

  if (x%a /= 42) error stop "Integer component not read correctly"
  if (trim(x%b) /= "hello") error stop "Character component not read correctly"

  print *, "OK"
end program p
