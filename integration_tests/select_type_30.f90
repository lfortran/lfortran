module select_type_30_mod

  implicit none

  type, abstract :: base_type
    integer :: id = 0
  end type base_type

  type, extends(base_type) :: derived_type
    real, allocatable :: values(:, :)
  end type derived_type

  type :: container_type
    class(base_type), allocatable :: obj
  end type container_type

end module select_type_30_mod


program select_type_30
  use select_type_30_mod
  implicit none

  type(container_type) :: c
  type(derived_type) :: d
  integer :: n

  allocate(d%values(3, 4))
  d%values = 1.0
  d%id = 42

  allocate(c%obj, source=d)

  n = 0
  select type(current => c%obj)
  type is (derived_type)
    if (current%id /= 42) error stop
    n = size(current%values)
  end select

  if (n /= 12) error stop
end program select_type_30