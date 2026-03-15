module submodule_37_mod
  implicit none
  private
  public :: mytype

  type mytype
    private
    character(len=:), allocatable :: val_
  contains
    procedure :: get_val
    generic :: get => get_int_array, get_real_array
    procedure, private :: get_int_array
    procedure, private :: get_real_array
  end type

  interface mytype
    elemental module function create(s) result(t)
      character(len=*), intent(in) :: s
      type(mytype) t
    end function
  end interface

  interface
    pure module function get_val(self) result(v)
      class(mytype), intent(in) :: self
      character(len=:), allocatable :: v
    end function

    pure module function get_int_array(self, mold) result(v)
      class(mytype), intent(in) :: self
      integer, intent(in) :: mold(:)
      integer, allocatable :: v(:)
    end function

    pure module function get_real_array(self, mold) result(v)
      class(mytype), intent(in) :: self
      real, intent(in) :: mold(:)
      real, allocatable :: v(:)
    end function
  end interface

end module submodule_37_mod
