program allocate_14
    implicit none

    type string
        character(:), allocatable :: str
    end type string

    type(string), allocatable :: inputs(:)
    inputs = [string("123"), string("456")]
    if ( inputs(2)%str /= "456" ) error stop
end program allocate_14
