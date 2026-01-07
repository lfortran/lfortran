program declaration_08
  implicit none
  type t
    integer :: i
  end type

  type(t) :: y = t(3)
  type(t) :: x = y
end program
