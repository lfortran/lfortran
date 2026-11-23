module select_type_71_mod
   implicit none
   type :: base_t
      integer :: n = 2
   end type
   type, extends(base_t) :: cart_t
      integer, allocatable :: d(:)
   contains
      procedure, pass(c) :: get => cart_get
   end type
contains
   subroutine cart_get(c, dims)
      class(cart_t), intent(in) :: c
      integer, intent(out) :: dims(c%n)
      dims = 7
   end subroutine
   subroutine cart_init(c)
      class(base_t), intent(inout) :: c
      select type (c)
      class is (cart_t)
         allocate(c%d(c%n))
         call c%get(c%d)
      end select
   end subroutine
end module

program select_type_71
   use select_type_71_mod
   implicit none
   type :: t
      integer :: n = 3
      integer :: d(3)
   end type
   type(t) :: x
   type(cart_t) :: y
   call init(x)
   if (any(x%d /= 5)) error stop
   call cart_init(y)
   if (size(y%d) /= 2) error stop
   if (any(y%d /= 7)) error stop
   print *, "ok"
contains
   subroutine get(c, d)
      type(t), intent(in) :: c
      integer, intent(out) :: d(c%n)
      d = 5
   end subroutine
   subroutine init(c)
      class(*), intent(inout) :: c
      select type (c)
      type is (t)
         call get(c, c%d)
      end select
   end subroutine
end program
