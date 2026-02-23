! Module with abstract interface used as procedure pointer type in a
! module function declaration. Tests that the abstract interface is
! properly serialized/deserialized when the submodule is loaded during
! separate compilation.
module submodule_27_m
  implicit none
  abstract interface
    pure function fn_i(x) result(v)
      real(8), intent(in) :: x
      real(8) :: v
    end function
  end interface
  type :: vec_t
    real(8) :: val
  end type
  interface
    module function make_vec(fn) result(v)
      procedure(fn_i), pointer :: fn
      type(vec_t) :: v
    end function
  end interface
end module submodule_27_m
