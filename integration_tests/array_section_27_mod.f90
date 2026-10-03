module array_section_27_mod
   type :: MyType
      real(8), pointer :: arr(:,:)
   end type MyType
   type(MyType) :: obj
end module array_section_27_mod
