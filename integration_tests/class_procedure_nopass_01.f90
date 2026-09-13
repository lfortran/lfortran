program class_procedure_nopass_01
    ! Type bound procedures declared outside of any module: the symbol table
    ! visitor must not consult an enclosing module symbol here.
    implicit none

    abstract interface
        subroutine gi()
        end subroutine gi
    end interface

    interface
        subroutine h()
        end subroutine h
    end interface

    type, abstract :: base_t
    contains
        procedure(gi), deferred, nopass :: g
    end type

    type :: mt
        integer :: i = 5
    contains
        procedure, nopass :: h
    end type

    type(mt) :: obj
    if (obj%i /= 5) error stop
end program

subroutine h()
end subroutine h
