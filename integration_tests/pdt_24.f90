program pdt_24
  implicit none
  type :: box(n)
    integer, len :: n
    integer :: first, second
  end type
  type(box(3)) :: a
  character(len=80) :: line
  integer :: first, second, extra, ios
  a%first = 13
  a%second = 29
  print *, a
  write(line, *) a
  read(line, *, iostat=ios) first, second
  if (ios /= 0 .or. first /= 13 .or. second /= 29) error stop 1
  read(line, *, iostat=ios) first, second, extra
  if (ios == 0) error stop 2
end program
