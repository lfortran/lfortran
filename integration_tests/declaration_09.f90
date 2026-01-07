program declaration_09
  implicit none
  type t
    integer :: i
  end type

  type(t), parameter :: arr(1) = [t(1)]
  type(t) :: x = arr
end program
