module traits_type_adoption_02_facade
    use traits_type_adoption_02_child, only: Concrete => Child
    use traits_type_adoption_02_parent, only: Base => Parent
    use traits_type_adoption_02_contracts, only: IAll, IValue, IExtra
    implicit none
    public
end module
