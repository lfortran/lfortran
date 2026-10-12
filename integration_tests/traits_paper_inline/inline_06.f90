module enhanced
   use traits_paper_inline_core, only: IConstructable, IPrintable
   use basic, only: MyType

   implements (IConstructable + IPrintable) :: MyType
   end implements MyType

end module enhanced
