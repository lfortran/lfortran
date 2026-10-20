! The interface body after the type gives the external procedure an explicit
! interface, so the component can name it in procedure().

subroutine procedure_interface_external_01_s(i)
    implicit none
    integer, intent(out) :: i
    i = 5
end subroutine

program procedure_interface_external_01
    implicit none
    type :: t
        procedure(procedure_interface_external_01_s), pointer, nopass :: c => null()
    end type
    interface
        subroutine procedure_interface_external_01_s(i)
            integer, intent(out) :: i
        end subroutine
    end interface
    type(t) :: x
    integer :: i
    i = 0
    x%c => procedure_interface_external_01_s
    call x%c(i)
    if (i /= 5) error stop
    print *, i
end program
