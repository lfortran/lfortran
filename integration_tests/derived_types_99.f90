program derived_types_99
  implicit none
  type :: t
     integer :: x
  end type
  type(t) :: a(4)
  integer :: i
  do i = 1, 4
     a(i)%x = i * 10
  end do
  associate(q => a%x)
     print *, q
  end associate
end program derived_types_99
