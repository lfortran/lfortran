module procedure_pointer_keyword_01_mod
  implicit none

  abstract interface
    function func_iface(x, verbose) result(res)
      integer, intent(in) :: x
      integer, intent(in), optional :: verbose
      integer :: res
    end function
  end interface

  type :: creator_type
    procedure(func_iface), pointer, nopass :: create_ptr => null()
  end type

contains

  function my_func(x, verbose) result(res)
    integer, intent(in) :: x
    integer, intent(in), optional :: verbose
    integer :: res
    res = x
    if (present(verbose)) res = res + verbose
  end function

end module

program procedure_pointer_keyword_01
  use procedure_pointer_keyword_01_mod
  implicit none
  type(creator_type) :: creator
  integer :: r

  creator%create_ptr => my_func
  r = creator%create_ptr(42, verbose=1)
  if (r /= 43) error stop
  print *, r
end program
