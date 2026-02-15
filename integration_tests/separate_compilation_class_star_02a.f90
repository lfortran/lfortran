module separate_compilation_class_star_02_mod

   type, abstract :: AbsGetter
   contains
      procedure(get), deferred :: get
   end type AbsGetter

   abstract interface
      function get(self, key) result(res)
         import
         class(AbsGetter), intent(in)  :: self
         character(*),     intent(in)  :: key
         class(*),         allocatable :: res
      end function get
   end interface

   type, abstract :: AbsType
   end type AbsType

   type :: MyType
      class(AbsType), allocatable :: ob
   end type MyType

contains

   function create(c) result(res)
      class(AbsGetter), intent(in)  :: c
      class(*),         allocatable :: res

      select type( obj => c%get("AbsType") )
      class is (AbsType)
         res = MyType(obj)
      end select

   end function create

end module separate_compilation_class_star_02_mod
