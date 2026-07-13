module submodule_55_mod
   implicit none
   type :: sample_t
      integer :: val = 42
   end type sample_t

   interface
      module subroutine do_it(n)
         integer, intent(in) :: n
      end subroutine do_it

      module function make_upoly(n) result(r)
         integer, intent(in) :: n
         class(*), allocatable :: r(:,:)
      end function make_upoly
   end interface
end module submodule_55_mod

submodule(submodule_55_mod) submodule_55_sub
contains
   module function make_upoly(n) result(r)
      integer, intent(in) :: n
      class(*), allocatable :: r(:,:)
      type(sample_t), allocatable :: tmp(:,:)
      allocate(tmp(n,1))
      allocate(r, source=tmp)
   end function make_upoly

   module subroutine do_it(n)
      integer, intent(in) :: n
      class(*), allocatable :: data_poly(:,:)
      data_poly = make_upoly(n)
      if (allocated(data_poly)) deallocate(data_poly)
   end subroutine do_it
end submodule submodule_55_sub

program submodule_55
   use submodule_55_mod, only: do_it
   implicit none
   integer :: dummy
   dummy = 0
   if (dummy /= 0) call do_it(dummy)
   print *, "ok"
end program submodule_55
