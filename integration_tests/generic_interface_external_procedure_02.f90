! An external procedure has an explicit interface in its own subprogram, so it
! can be a specific procedure of a generic interface declared there.

recursive subroutine countdown(i)
    implicit none
    integer, intent(inout) :: i
    interface gen
        procedure countdown
    end interface
    if (i > 0) then
        i = i - 1
        call gen(i)
    end if
end subroutine

program generic_interface_external_procedure_02
    implicit none
    integer :: i
    i = 3
    call countdown(i)
    if (i /= 0) error stop
    print *, i
end program
