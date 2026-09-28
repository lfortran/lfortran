! If the component-data-source of a scalar allocatable derived-type
! component in a structure constructor is an unallocated allocatable object,
! the corresponding component of the constructed value is unallocated
! (F2018 7.5.10). GFortran 13 miscompiles this (segfault).
module structure_constructor_args_24_mod
   implicit none

   type :: inner_t
      integer :: k = 0
      integer, allocatable :: w(:)
   end type inner_t

   type :: a_t
      integer :: x = 1
      type(inner_t), allocatable :: t
   end type a_t

   type :: outer_t
      type(a_t) :: a
      type(inner_t), allocatable :: u
   end type outer_t

   type :: p_t
      type(inner_t), allocatable :: p
      type(inner_t), allocatable :: q
   end type p_t

contains

   logical function t_allocated(a) result(res)
      type(a_t), intent(in) :: a
      res = allocated(a%t)
   end function t_allocated

end module structure_constructor_args_24_mod

program structure_constructor_args_24
   use structure_constructor_args_24_mod
   implicit none
   type(a_t) :: a, b
   type(outer_t) :: o
   type(inner_t), allocatable :: lt
   type(p_t) :: s
   type(a_t) :: arr(3)
   integer :: i

   a%x = 5
   b = a_t(a%x, a%t)
   if (b%x /= 5) error stop
   if (allocated(b%t)) error stop

   ! The target's component held a value before; it must be released.
   allocate(b%t)
   allocate(b%t%w(2))
   b = a_t(a%x, a%t)
   if (allocated(b%t)) error stop

   ! Nested constructors.
   o = outer_t(a_t(3, a%t), a%t)
   if (o%a%x /= 3) error stop
   if (allocated(o%a%t)) error stop
   if (allocated(o%u)) error stop

   ! A constructor passed as an actual argument.
   if (t_allocated(a_t(4, a%t))) error stop

   ! Allocated sources are still copied.
   allocate(a%t)
   a%t%k = 7
   allocate(a%t%w(3))
   a%t%w = [1, 2, 3]
   b = a_t(a%x, a%t)
   if (.not. allocated(b%t)) error stop
   if (b%t%k /= 7) error stop
   if (any(b%t%w /= [1, 2, 3])) error stop
   o = outer_t(a_t(3, a%t), a%t)
   if (o%a%t%k /= 7) error stop
   if (o%u%k /= 7) error stop
   if (.not. t_allocated(a_t(4, a%t))) error stop

   ! An unallocated local allocatable variable as the data source.
   b = a_t(2, lt)
   if (b%x /= 2) error stop
   if (allocated(b%t)) error stop

   ! The arguments read the assignment target: their values are taken
   ! before any component of the target is defined.
   allocate(s%p, s%q)
   s%p%k = 1
   s%q%k = 2
   s = p_t(s%q, s%p)
   if (s%p%k /= 2) error stop
   if (s%q%k /= 1) error stop
   deallocate(s%q)
   s = p_t(s%q, s%p)
   if (allocated(s%p)) error stop
   if (.not. allocated(s%q)) error stop
   if (s%q%k /= 2) error stop

   ! Executed repeatedly, an allocated data source must not leave its value
   ! behind for a later unallocated one.
   allocate(arr(1)%t)
   arr(1)%t%k = 8
   allocate(arr(3)%t)
   arr(3)%t%k = 10
   do i = 1, 3
      b = a_t(i, arr(i)%t)
      if (b%x /= i) error stop
      if (allocated(b%t) .neqv. (i /= 2)) error stop
      if (i /= 2) then
         if (b%t%k /= 7 + i) error stop
      end if
   end do

   print *, "ok"
end program structure_constructor_args_24
