module separate_compilation_47_a
   type, abstract :: AbsType
   end type AbsType

   type, extends(AbsType) :: AbsTypeDerived
      integer :: value
   end type AbsTypeDerived
end module separate_compilation_47_a
