module allocate_29_node_mod
  type :: dependency_config_t
      integer, allocatable :: arr(:)
  end type dependency_config_t

  type, extends(dependency_config_t) :: dependency_node_t
  end type dependency_node_t
end module allocate_29_node_mod


program allocate_29
  use allocate_29_node_mod
  implicit none

  type(dependency_node_t), allocatable :: a, b
  allocate(a, b)

  b = a

  ! arr should not be allocated
  if (allocated(b%arr)) stop 1
end program allocate_29