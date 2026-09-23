module m_shapes_mold
  implicit none
  type :: base_shape
    real :: x = 5
  end type base_shape
  type, extends(base_shape) :: circle_shape
    real :: r = 1
  end type circle_shape
end module m_shapes_mold

program allocatable_polymorphic_mold_02
  use m_shapes_mold
  implicit none
  class(*), allocatable :: u(:)
  class(circle_shape), allocatable :: blob

  ! MOLD= (unlike SOURCE=) is legal even when the mold variable is
  ! unallocated: F2018 9.7.1.2 only requires its declared type and
  ! shape, not its runtime value. Here `blob` is deliberately left
  ! unallocated.
  allocate(u(2), mold = blob)

  select type (u)
  type is (circle_shape)
    ! Default component initializers (including the inherited one
    ! from base_shape) must run for the array allocated via mold.
    if (u(1)%x /= 5.0 .or. u(1)%r /= 1.0) error stop
    if (u(2)%x /= 5.0 .or. u(2)%r /= 1.0) error stop
  class default
    error stop
  end select

  deallocate(u)
end program allocatable_polymorphic_mold_02
