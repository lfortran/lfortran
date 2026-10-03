! If the component-data-source of a scalar allocatable component in a
! structure constructor is an unallocated allocatable object, the
! corresponding component of the constructed value is unallocated
! (F2018 7.5.10). GFortran 13 miscompiles this (segfault for the integer
! component, allocated status for the character component).
program structure_constructor_args_23
   implicit none

   type :: a_t
      integer :: x = 1
      integer, allocatable :: s
      character(len=:), allocatable :: c
   end type a_t

   type(a_t) :: a, b
   integer, allocatable :: ls

   a%x = 5
   b = a_t(a%x, a%s, a%c)
   if (b%x /= 5) error stop
   if (allocated(b%s)) error stop
   if (allocated(b%c)) error stop

   ! The target's components held values before; they must be released.
   allocate(b%s)
   b%s = 3
   allocate(character(len=3) :: b%c)
   b%c = "old"
   b = a_t(a%x, a%s, a%c)
   if (allocated(b%s)) error stop
   if (allocated(b%c)) error stop

   ! Allocated sources are still copied.
   allocate(a%s)
   a%s = 11
   allocate(character(len=5) :: a%c)
   a%c = "hello"
   b = a_t(a%x, a%s, a%c)
   if (b%s /= 11) error stop
   if (b%c /= "hello") error stop

   ! An unallocated local allocatable variable as the data source.
   b = a_t(2, ls, a%c)
   if (b%x /= 2) error stop
   if (allocated(b%s)) error stop
   if (len(b%c) /= 5) error stop

   print *, "ok"
end program structure_constructor_args_23
