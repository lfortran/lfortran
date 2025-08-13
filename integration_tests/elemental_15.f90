module elemental_15_mod
  implicit none
  type :: t
     integer :: v
  contains
     procedure, pass :: neq
     generic :: operator(/=) => neq
  end type
contains
  elemental logical function neq(a,b)
    class(t), intent(in) :: a, b
    neq = a%v /= b%v
  end
end module

program elemental_15
  use elemental_15_mod
  type(t) :: a(2) = [t(1), t(2)], b(2) = [t(1), t(2)]
  if (any(a /= b)) error stop
  a(1)%v = 2
  if (.not. any(a /= b)) error stop
end program
