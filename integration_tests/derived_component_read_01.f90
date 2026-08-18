program derived_component_read_01
    implicit none
    type :: point
        integer :: province
    end type
    type(point) :: points(3)
    character(len=5) :: record
    integer :: i

    record = "2 4 6"
    read(record, *) (points(i)%province, i=1, 3)
    if (any(points%province /= [2, 4, 6])) error stop
end program
