module class_75_mod
  implicit none

  type :: x
    integer :: i = 66
  end type x

  type, extends(x) :: z
    integer :: z_a
  end type z

  type, extends(x) :: y
    integer, allocatable :: arr(:)
    character(len=:), allocatable :: str
  end type y

end module class_75_mod

program class_75
  use class_75_mod
  implicit none

  class(y), allocatable :: c, v
  type(y) :: yy
  allocate(c)
  yy%i = 42
  allocate(yy%arr(3))
  yy%arr = [1,2,3]
  yy%str = "hello"
  v = yy
  c = v
  select type(c)
    type is (y)
      print *, "i:", c%i
      print *, "arr:", c%arr
      print *, "str:", c%str
      if (c%i /= 42) error stop
      if (any(c%arr /= [1,2,3])) error stop
      if (c%str /= "hello") error stop
    class default
      error stop
  end select
end program class_75
