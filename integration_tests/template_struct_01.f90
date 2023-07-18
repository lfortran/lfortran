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

      function get_fst(p) result(r)
          type(tuple), intent(in) :: p
          type(t) :: r
          r = p%fst
      end function

      function get_snd(p) result(r)
        type(tuple), intent(in) :: p
        type(t) :: r
        r = p%snd
      end function

  end template

contains

  subroutine test_template()
      instantiate struct_t(integer), &
          only: int_tuple => tuple, get_int_fst => get_fst, &
                get_int_snd => get_snd
      type(int_tuple) :: t
      t%fst = 1
      print *, "First element: ", get_int_fst(t)
      t%snd = 2
      print *, "Second element: ", get_int_snd(t)
  end subroutine

end module

program template_struct_01
use template_struct_01_m

implicit none

call test_template()

end program template_struct_01