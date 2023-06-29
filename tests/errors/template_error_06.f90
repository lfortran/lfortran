module template_struct_02_m
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
          type(g) :: fst
          type(g) :: snd
      end type

  contains

  end template

contains

end module

program template_struct_02
use template_struct_02_m

implicit none

end program