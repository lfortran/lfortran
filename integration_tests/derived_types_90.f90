program derived_types_90
  implicit none
  
  type int_t
    integer :: i
  end type

  type(int_t) :: T_ONE, T_TWO, T_THREE
  type(int_t) :: T_ALL(3)
  
  T_ONE = int_t(1)
  T_TWO = int_t(2)
  T_THREE = int_t(3)
  
  T_ALL = (/ T_ONE, T_TWO, T_THREE /)
  
  if (T_ALL(1)%i /= 1) error stop "T_ALL(1)%i should be 1"
  if (T_ALL(2)%i /= 2) error stop "T_ALL(2)%i should be 2"
  if (T_ALL(3)%i /= 3) error stop "T_ALL(3)%i should be 3"
end program derived_types_90
