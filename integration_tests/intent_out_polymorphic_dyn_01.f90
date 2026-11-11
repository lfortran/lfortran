module intent_out_polymorphic_dyn_01_local
   use intent_out_polymorphic_dyn_01_base
   implicit none
   type, extends(base) :: local_ext
      real, allocatable :: e(:)
   end type
end module

program intent_out_polymorphic_dyn_01
   use intent_out_polymorphic_dyn_01_base
   use intent_out_polymorphic_dyn_01_ext
   use intent_out_polymorphic_dyn_01_local
   implicit none
   type(ext) :: x
   type(ext2) :: y
   type(local_ext) :: z
   type(plain_ext) :: p
   type(base) :: ba(2)
   class(base), allocatable :: cb
   integer :: i

   ! an extension declared in another module
   call init(x)
   if (allocated(x%a) .or. .not. allocated(x%b)) error stop 1
   call init(x)
   call init(x)
   if (x%s /= "abc") error stop 2

   ! a chain of two extensions, with allocatable components at every level
   allocate (y%a(3), y%b(3), y%c, y%inner%a(1), y%inner%b(1))
   call destroy(y)
   if (allocated(y%a) .or. allocated(y%b) .or. allocated(y%c)) error stop 3
   if (allocated(y%s) .or. allocated(y%inner%a) .or. allocated(y%inner%b)) error stop 4
   allocate (y%b(2), y%c, y%a(1))
   y%c = 1
   call destroy(y)
   if (allocated(y%a) .or. allocated(y%b) .or. allocated(y%c)) error stop 5
   allocate (y%b(4), y%a(2))
   if (size(y%b) /= 4) error stop 6

   ! an extension declared in the module of the program
   allocate (z%a(2), z%e(2))
   call destroy(z)
   if (allocated(z%a) .or. allocated(z%e)) error stop 7
   allocate (z%e(3))
   call destroy(z)
   allocate (z%e(1))
   if (size(z%e) /= 1) error stop 8

   ! an allocatable class entity
   allocate (ext2 :: cb)
   allocate (cb%a(2))
   select type (cb)
   type is (ext2)
      allocate (cb%b(2), cb%c)
      call destroy(cb)
      if (allocated(cb%a) .or. allocated(cb%b) .or. allocated(cb%c)) error stop 9
      allocate (cb%b(1))
   end select
   deallocate (cb)

   ! the dynamic type is the declared type
   allocate (ba(1)%a(2))
   call destroy(ba(1))
   if (allocated(ba(1)%a)) error stop 10

   ! absent optional
   call destroy_optional()
   deallocate (x%b)
   allocate (x%b(1), x%a(1))
   call destroy_optional(x)
   if (allocated(x%a) .or. allocated(x%b)) error stop 11

   ! a declared type without allocatable components
   allocate (p%d(2))
   p%n = 3
   call destroy_plain(p)
   if (allocated(p%d)) error stop 12
   if (p%n /= 0) error stop 13

   ! unlimited polymorphic
   call destroy_unlimited(i)

   ! array of the declared type
   allocate (ba(1)%a(1), ba(2)%a(1))
   call destroy_array(ba)
   do i = 1, 2
      if (allocated(ba(i)%a)) error stop 14
   end do

   print *, "ok"
end program
