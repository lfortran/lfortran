! If the component-data-source of an allocatable component in a structure
! constructor is an unallocated allocatable object, the corresponding
! component of the constructed value is unallocated (F2018 7.5.10).
module structure_constructor_args_22_mod
   implicit none

   type :: inner_t
      integer :: k = 0
   end type inner_t

   type :: a_t
      integer :: x = 1
      integer, allocatable :: v(:)
      real, allocatable :: r(:,:)
      character(len=2), allocatable :: c(:)
      type(inner_t), allocatable :: t(:)
   end type a_t

   type :: outer_t
      type(a_t) :: a
      integer, allocatable :: w(:)
   end type outer_t

contains

   logical function any_allocated(a) result(res)
      type(a_t), intent(in) :: a
      res = allocated(a%v) .or. allocated(a%r) .or. allocated(a%c) &
         .or. allocated(a%t)
   end function any_allocated

end module structure_constructor_args_22_mod

program structure_constructor_args_22
   use structure_constructor_args_22_mod
   implicit none
   type(a_t) :: a, b
   type(outer_t) :: o
   integer, allocatable :: lv(:)

   ! Unallocated components of another object.
   a%x = 5
   b = a_t(a%x, a%v, a%r, a%c, a%t)
   if (b%x /= 5) error stop
   if (any_allocated(b)) error stop

   ! The target's components held values before; they must be released.
   allocate(b%v(3), b%r(2,2), b%c(2), b%t(2))
   b = a_t(a%x, a%v, a%r, a%c, a%t)
   if (b%x /= 5) error stop
   if (any_allocated(b)) error stop

   ! Allocated sources are still copied.
   allocate(a%v(3), a%r(2,3), a%c(2), a%t(2))
   a%v = [1, 2, 3]
   a%r = 1.5
   a%c = ["ab", "cd"]
   a%t = [inner_t(7), inner_t(8)]
   b = a_t(a%x, a%v, a%r, a%c, a%t)
   if (.not. allocated(b%v)) error stop
   if (any(b%v /= [1, 2, 3])) error stop
   if (any(shape(b%r) /= [2, 3])) error stop
   if (any(b%r /= 1.5)) error stop
   if (size(b%c) /= 2) error stop
   if (b%c(2) /= "cd") error stop
   if (size(b%t) /= 2) error stop
   if (b%t(2)%k /= 8) error stop

   ! An unallocated local allocatable variable as the data source.
   b = a_t(2, lv, a%r, a%c, a%t)
   if (b%x /= 2) error stop
   if (allocated(b%v)) error stop
   if (.not. allocated(b%r)) error stop

   ! Nested constructors.
   deallocate(a%v, a%r)
   o = outer_t(a_t(3, a%v, a%r, a%c, a%t), lv)
   if (o%a%x /= 3) error stop
   if (allocated(o%a%v)) error stop
   if (allocated(o%a%r)) error stop
   if (size(o%a%c) /= 2) error stop
   if (allocated(o%w)) error stop

   ! A constructor passed as an actual argument.
   deallocate(a%c, a%t)
   if (any_allocated(a_t(4, a%v, a%r, a%c, a%t))) error stop
   if (.not. any_allocated(a_t(4, a%v, a%r, a%c, [inner_t(1)]))) error stop

   print *, "ok"
end program structure_constructor_args_22
