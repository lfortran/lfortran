module generic_name_03_mod
  use iso_c_binding, only: c_funptr, c_funloc
  implicit none

  abstract interface
    integer function my_func_i()
    end function
  end interface

  type :: wrapper_t
    integer :: x
  end type

  interface wrapper_t
    module procedure construct_from_int
    module procedure construct_from_funloc
  end interface

contains

  pure function construct_from_int(x) result(res)
    integer, intent(in) :: x
    type(wrapper_t) :: res
    res%x = x
  end function

  pure function construct_from_funloc(p) result(res)
    type(c_funptr), intent(in) :: p
    type(wrapper_t) :: res
    res%x = 0
  end function

end module

program generic_name_03
  use generic_name_03_mod
  implicit none
  type(wrapper_t) :: w
  procedure(my_func_i), pointer :: fptr
  fptr => my_func
  w = wrapper_t(c_funloc(fptr))
  print *, w%x
  if (w%x /= 0) error stop
  w = wrapper_t(42)
  print *, w%x
  if (w%x /= 42) error stop
contains
  integer function my_func()
    my_func = 42
  end function
end program
