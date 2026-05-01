program array_section_27
   ! Regression test for issue #11293:
   ! Across separate-compilation translation units, the LLVM finalizer used a
   ! `static` dummy ASR Var node tied to the allocator of the *first*
   ! compilation unit; once that allocator was destroyed, subsequent calls
   ! to get_llvm_type during finalize_symtab dereferenced freed memory and
   ! the compiler aborted with `AssertFailed: is_a<T>(*f)`.
   use array_section_27_mod
   implicit none
   interface
      subroutine array_section_27_set(arr)
         real(8), dimension(:) :: arr
      end subroutine array_section_27_set
   end interface
   allocate( obj%arr(2,2) )
   obj%arr = 0.0d0
   call array_section_27_set( obj%arr(:,1) )
   write(*,*) "main:", obj%arr(1,1), obj%arr(2,1)
   if (abs(obj%arr(1,1) - 1.111d0) > 1.0d-12) error stop
   if (abs(obj%arr(2,1) - 2.222d0) > 1.0d-12) error stop
   if (abs(obj%arr(1,2)) > 1.0d-12) error stop
   if (abs(obj%arr(2,2)) > 1.0d-12) error stop
end program array_section_27
