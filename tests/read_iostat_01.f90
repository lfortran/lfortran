program read_iostat_01
    implicit none
    real(8) :: d
    integer :: i
    read(*, *, iostat=i) d
    print *, i
end program read_iostat_01
