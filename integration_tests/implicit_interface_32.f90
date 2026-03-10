program implicit_interface_32
  implicit none

  integer :: my_fn
  external :: my_fn

  integer :: i, j

  integer :: my_stfn

  my_stfn (i) = my_fn (i) + 1

  j = my_stfn (41)
  if (j /= 42) error stop

end program

integer function my_fn (i)
  integer :: i

  my_fn = i

end function
