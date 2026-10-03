program logical_array_cast_01
    implicit none
    call activate()
contains
    subroutine activate()
        logical(1), dimension(2) :: iremove = [.false., .true.]
        if (iremove(1) .or. .not. iremove(2)) error stop
    end subroutine activate
end program logical_array_cast_01
