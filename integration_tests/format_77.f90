program format_77
    implicit none

    logical :: value
    character(len=10) :: buffer
    integer :: stat

    value = .true.
    stat = 0
    write(buffer, "(1x)", iostat=stat) value
    if (stat == 0) error stop "expected non-zero iostat for excess output list item"
end program format_77
