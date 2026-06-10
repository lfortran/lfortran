program pdt_17
  implicit none

  type :: label(size)
    integer, len :: size
    character(len=size) :: text
  end type

  type(label(5)) :: entry

  entry%text = "hello"

  if (len(entry%text) /= 5) error stop "unexpected pdt len parameter"
  if (entry%text /= "hello") error stop "unexpected pdt character value"

  print *, "test passed."
end program pdt_17
