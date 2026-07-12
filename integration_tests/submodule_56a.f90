program submodule_56a
  use intrinsic_array_m_submodule_56, only : intrinsic_array_t
  implicit none
  integer, parameter :: a(*) = [1, 2, 3]
  type(intrinsic_array_t) :: x
  x = intrinsic_array_t(a)
  if (x%as_character() /= "ok") error stop
  if (size(x%integer_1D) /= 3) error stop
  if (any(x%integer_1D /= a)) error stop
  print *, "ok"
end program
