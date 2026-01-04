program read_end_label_undefined
    implicit none
    integer :: val
    ! Label 999 is not defined
    read(*, *, end=999) val  
    print *, val
end program
