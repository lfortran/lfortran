module procedure_mod
contains
    subroutine square(a)
        integer, intent(inout) :: a
        a = a * 2
    end subroutine square
end module procedure_mod

module class_mod
    use procedure_mod
    implicit none

    type :: my_type
    contains
        procedure, public, nopass :: square
    end type my_type
end module class_mod

program class_procedure_use_assoc_nopass_01
    use class_mod
    implicit none

    type(my_type) :: t
    integer :: i

    i = 2
    call t%square(i)
    if (i /= 4) error stop
end program class_procedure_use_assoc_nopass_01