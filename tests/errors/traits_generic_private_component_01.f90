! A private component of a generic type loaded from its module file stays
! private to the module that defines the generic type in an instance the main
! program owns (TD-37): the initializer gives it a value, but a structure
! constructor of the program cannot.
program traits_generic_private_component_01
    use traits_generic_private_component_lib, only: Holder
    implicit none
    type(Holder{integer}) :: h
    h = Holder{integer}(3, 1)
    h = Holder{integer}(value=3)
end program traits_generic_private_component_01
