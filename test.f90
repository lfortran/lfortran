module m
  type :: t
    integer :: i = 0
    type(t), pointer :: p => null()
  end type
end module
