program extern_fn
  implicit none
! Compile with --implicit-interface
  integer :: my_fn
  external :: my_fn
  integer :: i, j
  integer :: my_stfn
  my_stfn (i) = my_fn (i) + 1
  j = my_stfn (41)
  if (j /= 42) error stop "statement function with implicit interface failed"
  print *, "test passed"
end program

integer function my_fn (i)
  integer :: i
  my_fn = i
end function
