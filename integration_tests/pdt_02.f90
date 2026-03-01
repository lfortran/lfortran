module pdt_02_mod
  type :: container(rk, ik)
    integer, kind :: rk = 4
    integer, kind :: ik = 8
    integer(kind=ik)  :: i_val(20)
    real(kind=rk)     :: r_val(20)
  end type container
end module

program pdt_02
  use pdt_02_mod  
  implicit none

  type(container) :: obj1
  type(container(8,4)) :: obj2
  type(container(8)) :: obj3

  if (kind(obj1%i_val) /= 8 .or. obj1%ik /= 8) error stop
  if (kind(obj1%r_val) /= 4 .or. obj1%rk /= 4) error stop
  if (kind(obj2%i_val) /= 4 .or. obj2%ik /= 4) error stop
  if (kind(obj2%r_val) /= 8 .or. obj2%rk /= 8) error stop
  if (kind(obj3%i_val) /= 8 .or. obj3%ik /= 8) error stop
  if (kind(obj3%r_val) /= 8 .or. obj3%rk /= 8) error stop
end program pdt_02