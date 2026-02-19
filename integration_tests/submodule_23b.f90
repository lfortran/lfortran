module submodule_23_mod
  implicit none
  type :: mytype
  contains
    procedure, nopass :: check
  end type
  interface
    module function check(args) result(found)
      integer, intent(in) :: args(:)
      logical found
    end function
  end interface
end module