module types_m
  implicit none

  type int_t
    integer :: i
  end type

  type(int_t), parameter ::  &
    T_ONE = int_t(1),  &
    T_TWO = int_t(2),  &
    T_THREE = int_t(3)

  type(int_t), parameter ::  &
    T_ALL(3) = (/ T_ONE, T_TWO, T_THREE /)

end module types_m

program test
  use types_m
  implicit none
  print *, "T_ALL(1)%i =", T_ALL(1)%i
  print *, "T_ALL(2)%i =", T_ALL(2)%i
  print *, "T_ALL(3)%i =", T_ALL(3)%i
end program test
