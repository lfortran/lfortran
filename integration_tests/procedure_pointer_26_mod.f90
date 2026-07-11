module prcedure_pointer_26_mod_a
    implicit none
    type :: type_a
        procedure(), nopass, pointer :: caller => null()
    end type
end module

module procedure_pointer_26_mod_b
    use prcedure_pointer_26_mod_a
    implicit none
    integer :: state = 0
    type :: type_b
        procedure(), nopass, pointer :: caller => null()
    end type
end module
