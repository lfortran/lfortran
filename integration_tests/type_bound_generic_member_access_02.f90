module type_bound_generic_member_access_02_m
  implicit none

  type parent_t
    integer :: call_count = 0
  contains
    procedure, private :: divergence_1D_weights
  end type parent_t

  type, extends(parent_t) :: divergence_1D_t
  contains
    generic :: weights => divergence_1D_weights
  end type divergence_1D_t

contains

  subroutine divergence_1D_weights(self)
    class(parent_t), intent(inout) :: self
    self%call_count = self%call_count + 1
  end subroutine divergence_1D_weights

end module type_bound_generic_member_access_02_m

program type_bound_generic_member_access_02
  use type_bound_generic_member_access_02_m
  implicit none

  type(divergence_1D_t) :: obj
  call obj%weights()
  if (obj%call_count /= 1) error stop 1
end program type_bound_generic_member_access_02
