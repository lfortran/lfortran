program declaration_10
  implicit none
  type t
    integer :: i
  end type

  type(t), parameter :: p
  type(t) :: x = p
end program
