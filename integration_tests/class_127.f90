! Test: value-attribute arg passed to deferred procedure expecting intent(in)
module class_127_mod
  use iso_c_binding, only: c_double, c_ptr, c_int, c_f_pointer, c_loc
  implicit none

  type, abstract :: callbacks_t
  contains
    procedure(cb_double_iface), deferred :: double_value
  end type

  abstract interface
    integer function cb_double_iface(this, value)
      import callbacks_t, c_double
      class(callbacks_t) :: this
      real(c_double), intent(in) :: value
    end function
  end interface

  type, extends(callbacks_t) :: my_callbacks
  contains
    procedure :: double_value => my_double_value
  end type

  type :: box_type
    class(callbacks_t), pointer :: cb => null()
  end type

contains

  integer function my_double_value(this, value)
    class(my_callbacks) :: this
    real(c_double), intent(in) :: value
    my_double_value = int(2.0d0 * value)
  end function

  integer(c_int) function wrapper(ctx, dval) bind(c, name='')
    type(c_ptr), value :: ctx
    real(c_double), value :: dval
    type(box_type), pointer :: box
    call c_f_pointer(ctx, box)
    wrapper = box%cb%double_value(dval)
  end function

end module

program class_127
  use iso_c_binding, only: c_ptr, c_loc, c_double, c_int
  use class_127_mod
  implicit none

  type(my_callbacks), target :: cb
  type(box_type), target :: box
  type(c_ptr) :: ctx
  integer(c_int) :: res

  box%cb => cb
  ctx = c_loc(box)
  res = wrapper(ctx, 21.0_c_double)
  if (res /= 42) error stop
  print *, "ok"
end program
