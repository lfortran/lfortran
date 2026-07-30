module allocate_source_ptr_02_mod
   implicit none
   type :: t
      character(len=:), pointer :: p => null()
   end type
contains
   subroutine clone(dst, src)
      type(t), allocatable, intent(out) :: dst
      type(t), intent(in) :: src
      allocate(dst, source=src)
   end subroutine
end module

program allocate_source_ptr_02
   use allocate_source_ptr_02_mod
   implicit none
   type(t) :: a
   type(t), allocatable :: b
   character(len=5), target :: buf

   ! Unassociated character pointer component: source= must not try to
   ! copy through the null data pointer.
   call clone(b, a)
   if (.not. allocated(b)) error stop "b should be allocated"
   if (associated(b%p)) error stop "b%p should be unassociated"
   deallocate(b)

   ! Associated character pointer component: the copy must still happen.
   buf = "hello"
   a%p => buf
   call clone(b, a)
   if (.not. allocated(b)) error stop "b should be allocated"
   if (.not. associated(b%p)) error stop "b%p should be associated"
   if (b%p /= "hello") error stop "b%p should be 'hello'"

   print *, "PASS"
end program
