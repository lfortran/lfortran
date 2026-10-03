program arrays_126
    implicit none
    character(len=5), allocatable :: narrow(:)
    narrow = ["hello"]
    narrow = [narrow, " " // paragraph("world")]
    if (narrow(1) /= 'hello') error stop
    if (narrow(2) /= ' worl') error stop

contains

    function paragraph(s) result(res)
        character(len=*), intent(in) :: s
        character(len=5) :: res(1)
        res(1) = s
    end function

end program

