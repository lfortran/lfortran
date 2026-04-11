module associate_46_mod
   implicit none
   type :: BaseType
      integer :: val = 0
   end type BaseType
   type :: MyType
      class(BaseType), allocatable :: baseobj
   end type MyType
   type :: WrapperType
      class(MyType), allocatable :: myobj(:)
   end type WrapperType
end module associate_46_mod

program main
   use associate_46_mod
   implicit none
   type(WrapperType) :: wrap
   class(MyType), allocatable :: myobj(:)
   allocate(MyType :: myobj(2))
   allocate(wrap%myobj, source = myobj)

   ! Associate with unallocated polymorphic member (should not crash)
   associate( obj1 => wrap%myobj(1)%baseobj )
   end associate

   ! Now allocate baseobj and verify associate works with allocated member
   allocate(BaseType :: wrap%myobj(1)%baseobj)
   select type(b => wrap%myobj(1)%baseobj)
   type is (BaseType)
      b%val = 42
   end select

   associate( obj2 => wrap%myobj(1)%baseobj )
      select type(obj2)
      type is (BaseType)
         if (obj2%val /= 42) error stop
         print *, obj2%val
      class default
         error stop
      end select
   end associate

   print *, "ok"
end program main
