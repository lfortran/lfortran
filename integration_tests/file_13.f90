program file_13
    implicit none

    integer :: num
    character(:), allocatable :: filename, form, status, access


    open(UNIT=1, file="file_01_data.txt", form="formatted", access="stream", status="old")
    read(1, *) num
    close(1)

    print *, num
    if (num /= 10130) error stop

    allocate(character(16) :: filename)
    allocate(character(9) :: form)
    allocate(character(6) :: status)
    allocate(character(3) :: access)

    filename = "file_03_data.txt"
    form = "formatted"
    access = "stream"
    status = "old"

    open(UNIT=1, file=filename, form=form, access=access, status=status, pad="no")
    read(1, *) num
    close(1)

    print *, num
    if (num /= 10) error stop

end program
