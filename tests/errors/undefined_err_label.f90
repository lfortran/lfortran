program read_end_label_undefined
    implicit none
    integer :: val
    ! Label 500 is not defined
    read(*, *, err=500) val  
    print *, val
end program
