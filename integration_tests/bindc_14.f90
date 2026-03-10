module bindc_14_mod
  use, intrinsic :: iso_c_binding
  implicit none

  type, abstract :: base_type
  contains
    procedure(cb_no_args), deferred :: start_map
    procedure(cb_string),  deferred :: map_key
  end type

  abstract interface
    integer function cb_no_args(this)
      import base_type
      class(base_type) :: this
    end function
    integer function cb_string(this, value)
      import base_type, c_char
      class(base_type) :: this
      character(*,kind=c_char), intent(in) :: value
    end function
  end interface

  type :: box_type
    class(base_type), pointer :: callbacks => null()
  end type

contains

  integer(c_int) function foo(ctx) bind(c, name='')
    type(c_ptr), value :: ctx
    type(box_type), pointer :: box
    call c_f_pointer(ctx, box)
    foo = box%callbacks%start_map()
  end function

  integer(c_int) function bar(ctx, sVal, sLen) bind(c, name='')
    type(c_ptr), value :: ctx, sVal
    integer(c_size_t), value :: sLen
    character(len=sLen,kind=c_char), pointer :: fstr
    type(box_type), pointer :: box
    call c_f_pointer(sVal, fstr)
    call c_f_pointer(ctx, box)
    bar = box%callbacks%map_key(fstr)
  end function

end module

program bindc_14
  use bindc_14_mod
  implicit none
  print *, "ok"
end program
