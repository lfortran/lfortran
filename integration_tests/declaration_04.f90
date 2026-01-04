! Handle assignment from parameters of struct type
! Inside Declaration using type() syntax
program declaration_04
  implicit none
  type class_t
    integer :: i
    character(len=5) :: s
  end type
  type(class_t) , parameter :: T_ONE = class_t(15,'abcde')
  type(class_t) :: my_class = T_ONE

  print *, my_class
  if ((my_class%i) /= 15) error stop
  if ((my_class%s) /= 'abcde') error stop

end program declaration_04