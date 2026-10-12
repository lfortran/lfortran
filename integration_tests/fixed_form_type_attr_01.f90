      module fixed_form_type_attr_01_mod
      use iso_c_binding, only: c_int
      implicit none
      private
      public :: base, pt, child, cpt, multi, abs_t, impl_t, get_priv

      type, public :: pt
        integer :: a = 1
      end type pt

      type, abstract :: abs_t
        integer :: z = 0
      contains
        procedure :: getz => abs_getz
      end type

      type, extends(abs_t) :: impl_t
      contains
        procedure :: getz => impl_getz
      end type impl_t

      type :: base
        integer :: v = 10
      contains
        procedure :: get => base_get
      end type base

      type, extends(base) :: child
        integer :: w = 20
      contains
        procedure :: get => child_get
      end type child

      type, bind(c) :: cpt
        integer(c_int) :: x = 3
      end type cpt

      type, public, extends(pt) :: multi
        integer :: b = 2
      endtype

      type,private::priv
        integer :: p = 7
      end type priv

      contains

      integer function abs_getz(self)
      class(abs_t), intent(in) :: self
      abs_getz = self%z
      end function

      integer function impl_getz(self)
      class(impl_t), intent(in) :: self
      impl_getz = self%z + 100
      end function

      integer function base_get(self)
      class(base), intent(in) :: self
      base_get = self%v
      end function

      integer function child_get(self)
      class(child), intent(in) :: self
      child_get = self%v + self%w
      end function

      integer function get_priv()
      type(priv) :: q
      get_priv = q%p
      end function
      end module fixed_form_type_attr_01_mod

      program fixed_form_type_attr_01
      use fixed_form_type_attr_01_mod
      implicit none
      type, extends(child) :: local_t
        integer :: u = 5
      end type local_t
      type(pt) :: a
      type(multi) :: m
      type(cpt) :: c
      type(impl_t) :: i
      type(local_t) :: l
      class(base), allocatable :: obj
      integer :: k

      if (a%a /= 1) error stop 1
      if (m%a /= 1 .or. m%b /= 2) error stop 2
      if (c%x /= 3) error stop 3
      if (i%getz() /= 100) error stop 4
      if (get_priv() /= 7) error stop 5
      if (l%get() /= 30 .or. l%u /= 5) error stop 6

      allocate(child :: obj)
      k = obj%get()
      if (k /= 30) error stop 7
      print *, 'ok'
      end program
