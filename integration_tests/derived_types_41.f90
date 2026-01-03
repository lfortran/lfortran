module derived_types_41_mod
    implicit none

    type :: mytype
        integer :: a(2)
        integer :: b
        integer :: c(2)
    end type mytype

    type(mytype) :: mytype_instance = mytype([1, 2], 3, [4, 5])
 end module derived_types_41_mod

program derived_types_41
    use derived_types_41_mod

    if (any(mytype_instance%a /= [1, 2])) error stop
    if (mytype_instance%b /= 3) error stop
    if (any(mytype_instance%c /= [4, 5])) error stop
end program derived_types_41
