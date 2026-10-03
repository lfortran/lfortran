module generic_name_05_mod
  implicit none

  type :: SomeType
  end type SomeType

  interface SomeType
     procedure :: constructor
  end interface

contains

  function constructor() result(self)
    type(SomeType) :: self
  end function constructor

end module generic_name_05_mod


module mymod
  implicit none

  type :: MyType
  end type MyType

  interface MyType
     procedure :: constructor
  end interface

contains

  function constructor(a) result(self)
    real(8), optional, intent(in) :: a(:)
    type(MyType) :: self
  end function constructor

end module mymod


module generic_name_05_mod_2
  use generic_name_05_mod
  use mymod, only: MyType
  implicit none

  type :: ClientType
  end type ClientType

  interface ClientType
     procedure :: init
  end interface

contains

  function init() result(self)
    type(ClientType) :: self

    real(8) :: arr(6)
    class(MyType), allocatable :: obj

    arr = 1.0d0

    allocate(obj, source = MyType(a=arr))

    if (.not. allocated(obj)) then
       error stop "Allocation failed"
    end if

    if (any(arr /= 1.0d0)) then
       error stop "Array values were modified during allocation"
    end if

  end function init

end module generic_name_05_mod_2


program generic_name_05
  use generic_name_05_mod_2
  implicit none

  type(ClientType) :: c
  c = ClientType()
end program generic_name_05