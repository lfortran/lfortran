program coarray_operations_01
    implicit none
    integer :: a[*]
    integer :: me

    me = this_image()
    a = me
    sync all
    
    if (me == 1) then
        a = a[2]
        a[2] = me
    end if
    
    sync all
end program
