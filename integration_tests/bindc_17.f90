module bindc_17_mod
  use, intrinsic :: iso_c_binding
  implicit none

  type, bind(c) :: callbacks_t
    type(c_funptr) :: start = C_NULL_FUNPTR
    type(c_funptr) :: finish = C_NULL_FUNPTR
  end type

  type :: parser_t
    type(c_ptr) :: handle = C_NULL_PTR
    type(callbacks_t) :: cbs
  contains
    procedure :: init
    procedure :: run
    final :: parser_dealloc
  end type

  interface
    function c_alloc(cbs, ctx) result(h) bind(c, name='my_alloc')
      import callbacks_t, c_ptr
      type(callbacks_t) :: cbs
      type(c_ptr), value :: ctx
      type(c_ptr) :: h
    end function
    subroutine c_free(h) bind(c, name='my_free')
      import c_ptr
      type(c_ptr), value :: h
    end subroutine
    function c_run(h) result(r) bind(c, name='my_run')
      import c_ptr, c_int
      type(c_ptr), value :: h
      integer(c_int) :: r
    end function
  end interface

contains

  integer(c_int) function my_start(ctx) bind(c, name='')
    type(c_ptr), value :: ctx
    my_start = 42
  end function

  subroutine init(this)
    class(parser_t), intent(out), target :: this
    this%cbs%start = c_funloc(my_start)
    this%handle = c_alloc(this%cbs, C_NULL_PTR)
  end subroutine

  function run(this) result(r)
    class(parser_t), intent(inout) :: this
    integer :: r
    r = c_run(this%handle)
  end function

  subroutine parser_dealloc(this)
    type(parser_t) :: this
    if (c_associated(this%handle)) call c_free(this%handle)
    this%handle = C_NULL_PTR
  end subroutine

end module

program bindc_17
  use bindc_17_mod
  implicit none
  type(parser_t), target :: p
  integer :: r
  call p%init()
  r = p%run()
  if (r /= 42) error stop
  print *, "PASS"
end program
