module node_mod

  type :: dependency_config_t
      integer, allocatable :: arr(:)
  end type dependency_config_t
  type, extends(dependency_config_t) :: dependency_node_t
  end type dependency_node_t

end module node_mod


program demo
  use node_mod
  implicit none

  type(dependency_node_t), allocatable :: a, b
  allocate(a, b)
  b = a
  print *, allocated(b%arr)
  if (allocated(b%arr)) error stop
end program demo
