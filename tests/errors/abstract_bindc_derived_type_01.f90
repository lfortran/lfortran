module m
  implicit none
  type, abstract, bind(c) :: t
    integer :: x
  end type
end module
