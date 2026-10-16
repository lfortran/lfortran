! A generic interface that names an entry of the procedure that declares it
subroutine s(i)
    integer, intent(inout) :: i
    interface gen
        procedure e
    end interface
    i = i + 1
    if (i < 5) call gen(i)
    return
entry e(i)
    i = i + 10
end subroutine

program entry_generic_interface_02
    integer :: i
    i = 0
    call s(i)
    print *, i
    if (i /= 11) error stop
end program
