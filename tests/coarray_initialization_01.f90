program coarray_initialization_01
    implicit none
    integer :: a[*] = 5
    integer :: me

    me = this_image()

    sync all

    if (me == 1) then
        a = a[2] + 1
    end if

    sync all
end program
