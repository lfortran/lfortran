module intent_out_finalize_04_mod
   implicit none

   abstract interface
      subroutine test_iface(error)
         integer, intent(out) :: error
      end subroutine test_iface
   end interface

   type :: unittest_t
      character(len=:), allocatable :: name
      procedure(test_iface), pointer, nopass :: test => null()
   contains
      final :: destroy_unittest
   end type unittest_t

   abstract interface
      subroutine collect_iface(items)
         import :: unittest_t
         type(unittest_t), allocatable, intent(out) :: items(:)
      end subroutine collect_iface
   end interface

   type :: suite_t
      character(len=:), allocatable :: name
      procedure(collect_iface), pointer, nopass :: collect => null()
   contains
      final :: destroy_suite
   end type suite_t

contains

   subroutine destroy_unittest(self)
      type(unittest_t), intent(inout) :: self
      if (allocated(self%name)) deallocate(self%name)
      self%test => null()
   end subroutine destroy_unittest

   subroutine destroy_suite(self)
      type(suite_t), intent(inout) :: self
      if (allocated(self%name)) deallocate(self%name)
      self%collect => null()
   end subroutine destroy_suite

   function new_suite(n, c) result(self)
      character(len=*), intent(in) :: n
      procedure(collect_iface) :: c
      type(suite_t) :: self
      self%name = n
      self%collect => c
   end function new_suite

   subroutine collect_a(items)
      type(unittest_t), allocatable, intent(out) :: items(:)
      allocate(items(0))
   end subroutine collect_a

end module intent_out_finalize_04_mod

program intent_out_finalize_04
   use intent_out_finalize_04_mod
   implicit none
   type(suite_t), allocatable :: arr(:)
   allocate(arr(0))
   arr = [new_suite("a", collect_a)]
   if (size(arr) /= 1)         error stop 1
   if (arr(1)%name /= "a")     error stop 2
   if (.not. associated(arr(1)%collect)) error stop 3
   print *, "ok"
end program
