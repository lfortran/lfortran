! Acknowledges, from a separately compiled module, that the loaded MyType
! already has what the traits require (traits.tex inline-06), under a
! renamed import.
module traits_initializers_05_enhanced
    use traits_initializers_05_contracts, only: IConstructable, IValued
    use traits_initializers_05_basic, only: Thing => MyType
    implicit none

    implements (IConstructable + IValued) :: Thing
    end implements Thing

end module traits_initializers_05_enhanced
