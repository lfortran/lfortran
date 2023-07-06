module template_struct_01_m
  implicit none
  private
  public :: struct_t

  requirement r(t)
      type, deferred :: t
  end requirement

  template struct_t(t)
      requires r(t)
      private
      public :: tuple

      type :: tuple
          type(t) :: fst
          type(t) :: snd
      end type

  contains

      !function get_fst(p) result(r)
      !    type(tuple), intent(in) :: p
      !    type(t) :: r
      !    r = p%fst
      !end function

      !function get_snd(p) result(r)
      !  type(tuple), intent(in) :: p
      !  type(t) :: r
      !  r = p%snd
      !end function

  end template

contains

  subroutine test_template()
      instantiate struct_t(integer), only: int_tuple => tuple
      type(int_tuple) :: t
      t%fst = 1
      t%snd = 2
  end subroutine

end module

program template_struct_01
use template_struct_01_m

implicit none

end program template_struct_01