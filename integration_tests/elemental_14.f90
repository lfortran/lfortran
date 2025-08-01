module elemental_14_mod
  implicit none

  type :: number_t
    real :: val
  contains
    procedure :: is_positive
  end type number_t

  type :: dec 
    type(number_t), allocatable :: nums(:)
  end type
contains

  elemental logical function is_positive(self)
    class(number_t), intent(in) :: self
    is_positive = self%val > 0.0
  end function is_positive

end module elemental_14_mod

program elemental_14
  use elemental_14_mod
  implicit none

  type(number_t), dimension(5) :: nums
  type(dec) :: ele
  logical :: result
  integer :: i
  nums(1)%val = -1.0
  nums(2)%val = -2.0
  nums(3)%val = -3.0
  nums(4)%val = -5.0
  nums(5)%val = -6.0
  result = any(nums%is_positive())
  if (result .neqv. .false.) error stop
  nums(1)%val = 1.0
  result = any(nums%is_positive())
  if (result .neqv. .true.) error stop
  allocate(ele%nums(2))
  ele%nums(2)%val = -1.0
  ele%nums(1)%val = -1.0
  result = any(ele%nums%is_positive())
  if (result .neqv. .false.) error stop
end program elemental_14