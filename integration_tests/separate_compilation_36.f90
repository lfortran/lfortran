module separate_compilation_36b
   use separate_compilation_36a
   implicit none

   type :: MyType
      class(AbsType), allocatable :: obj
   end type MyType

contains

   subroutine client()
      class(MyType), allocatable :: arr(:,:)
      associate( ob => arr(1,1)%obj )
         call ob%ptr()
      end associate
   end subroutine client

end module separate_compilation_36b

program separate_compilation_36
   implicit none
   print *, "PASS"
end program separate_compilation_36
