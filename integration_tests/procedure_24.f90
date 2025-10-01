module procedure_24_mod
  implicit none

  type :: type1_t
  contains
    procedure :: printme
  end type type1_t

contains

  subroutine printme(this)
    class(type1_t), intent(in) :: this
  end subroutine printme

end module procedure_24_mod


module procedure_24_mod2
  implicit none
  type :: type2_t
    integer :: key
  contains
    procedure :: printme
  end type type2_t

contains

  subroutine printme(this, n)
    class(type2_t), intent(inout) :: this
    integer, intent(in) :: n
    this%key = n
  end subroutine printme

end module procedure_24_mod2

program procedure_24
  use procedure_24_mod
  use procedure_24_mod2
  implicit none

  type(type1_t) :: obj1
  type(type2_t) :: obj2

  call obj1%printme()
  call obj2%printme(422)

  if (obj2%key /= 422) error stop
end program procedure_24