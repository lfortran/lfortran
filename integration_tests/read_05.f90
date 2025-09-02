program main
    implicit none
    character(len=10) :: x
    ! Accept READ with format literal and no unit
    if (.false.) then
        read '(A)', x
    end if
    print *, 'ok'
end program
