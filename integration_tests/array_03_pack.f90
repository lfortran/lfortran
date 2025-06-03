program array_03_pack

    implicit none
    character(len=1), dimension(:,:), allocatable :: array
    integer :: packoutput(10)

    allocate(array(3, 5))
    array(1, :) = "a"
    array(2, :) = "."
    array(3, :) = "c"
    print *, ichar(pack(array, mask=(array /= '.')))
    packoutput = ichar(pack(array, mask=(array /= '.')))
    if( any(packoutput /= [97, 99, 97, 99, 97, 99, 97, 99, 97, 99]) ) error stop

end program
