program pointer_01
   implicit none

   integer, dimension(:), pointer :: int_ptr
   logical, dimension(:), pointer :: logical_ptr
   real, dimension(:, :), pointer :: real_ptr

   integer :: i

   nullify(int_ptr)
   print *, associated(int_ptr)
   if (associated(int_ptr) .neqv. .false.) error stop

   allocate(int_ptr(3))

   int_ptr = [1, 2, 3]
   print *, int_ptr
   print *, associated(int_ptr)
   if (associated(int_ptr) .neqv. .true.) error stop

   if (product(int_ptr) == 6) then
      nullify(int_ptr)

      allocate(int_ptr(5))
      int_ptr = (/ (i, i=1, 5)/)
      print *, int_ptr

      if (size(int_ptr) /= 5) error stop
   end if


   nullify(logical_ptr)
   print *, associated(logical_ptr)
   if (associated(logical_ptr) .neqv. .false.) error stop

   allocate(logical_ptr(5))

   logical_ptr = [.true., .false., .true., .false., .false.]
   print *, logical_ptr
   print *, associated(logical_ptr)
   if (associated(logical_ptr) .neqv. .true.) error stop

   if (size(logical_ptr) == 5) then
      nullify(logical_ptr)

      allocate(logical_ptr(2))
      logical_ptr = .true.
      print *, logical_ptr

      if (all(logical_ptr .neqv. [.true., .true.])) error stop
   end if


   nullify(real_ptr)
   print *, associated(real_ptr)
   if (associated(real_ptr) .neqv. .false.) error stop

   allocate(real_ptr(2, 2))

   real_ptr = reshape([1.0, 2.0, 3.0, 4.0], [2, 2])
   print *, real_ptr
   print *, associated(real_ptr)
   if (associated(real_ptr) .neqv. .true.) error stop

   if (sum(real_ptr) /= 11.0) then
      nullify(real_ptr)
   end if


   ! Uncomment after fixing #4506

   !    call sub

   ! contains

   !    subroutine sub

   !       nullify(int_ptr)
   !       print *, associated(int_ptr)

   !       allocate(int_ptr(2))

   !       int_ptr = [1, 2]
   !       print *, int_ptr
   !       print *, associated(int_ptr)

   !    end subroutine sub

end program pointer_01
