module derived_types_68_mod
    type :: version_t
      integer :: major
    contains
        generic :: operator(==) => equals
        procedure, private :: equals
    end type version_t

contains 
    logical function equals(a, b)
        class(version_t), intent(in) :: a, b
        equals = a%major == b%major
    end function equals
end module derived_types_68_mod

program derived_types_68
  use derived_types_68_mod, only: version_t
  type :: new_type
    type(version_t) :: version
  end type new_type
  type(new_type) :: obj1, obj2
  obj1%version%major = 1
  obj2%version%major = 5
  if (obj1%version == obj2%version) error stop
end program