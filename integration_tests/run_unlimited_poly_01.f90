
program unlimited_poly
    implicit none
    class(*), allocatable :: u
    
    ! This triggers the crash
    allocate(integer :: u)
    
    select type(u)
        type is (integer)
            print *, 'It is an integer!'
            u = 42
            print *, 'Value is:', u
        class default
            print *, 'Unknown type'
    end select
end program

