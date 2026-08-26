program test_array_struct
    implicit none
    
    type :: MatrixData
        integer :: id
        real(8) :: coords(3)
        integer :: matrix(2, 2)
    end type MatrixData
    
    type(MatrixData) :: md
    
    if (this_image() == 1) then
        md%id = 42
        md%coords = [1.1_8, 2.2_8, 3.3_8]
        md%matrix = reshape([1, 2, 3, 4], [2, 2])
    else
        md%id = 0
        md%coords = 0.0_8
        md%matrix = 0
    end if
    
    call co_broadcast(md, 1)
    
    sync all
    print *, "Image", this_image(), "id =", md%id, "coords(2) =", md%coords(2), "matrix(2,2) =", md%matrix(2,2)
end program test_array_struct