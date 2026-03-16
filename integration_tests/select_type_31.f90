module select_type_31_mod

  implicit none
  public :: derived_type
  public :: container_type
  public :: base_type

  type, abstract :: base_type
    character(:), allocatable :: name
  end type base_type

  type, extends(base_type) :: derived_type
    real, allocatable :: values(:, :)
  end type derived_type

  type :: container_type
    class(base_type), allocatable :: obj
    integer, allocatable :: dims(:)
  contains
    procedure :: initialize
  end type container_type

contains

  impure elemental module subroutine initialize(self, other)
    implicit none
    class(container_type), intent(in out) :: self
    class(container_type), intent(in) :: other

    select type(current => self%obj)
    type is (derived_type)
      self%dims = shape(current%values)
    end select

  end subroutine initialize

end module select_type_31_mod


program select_type_31
  use select_type_31_mod
  implicit none

  type(container_type) :: c1, c2
  type(derived_type) :: d
  d%name = "test"
  allocate(d%values(3, 4))
  d%values = 1.0

  allocate(c1%obj, source=d)
  allocate(c2%obj, source=d)
  call c1%initialize(c2)

  if (.not. allocated(c1%dims)) error stop "c1%dims not allocated"
  if (size(c1%dims) /= 2) error stop "c1%dims should have 2 elements"
  if (c1%dims(1) /= 3) error stop "c1%dims(1) should be 3"
  if (c1%dims(2) /= 4) error stop "c1%dims(2) should be 4"

  select type (obj => c1%obj)
  type is (derived_type)
    if (obj%name /= "test") error stop "name should be 'test'"
  class default
    error stop "unexpected type in c1%obj"
  end select
end program select_type_31
