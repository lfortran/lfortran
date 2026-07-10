program coarray_saved_01
    implicit none
    integer, save :: a[*]
    integer :: me

    me = this_image()
    a = me

    sync all

    if (me == 1) then
        a = a[2]
    end if

    sync all
end program
