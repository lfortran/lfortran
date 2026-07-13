program coarray_collectives_01
    implicit none
    integer :: me, n_images
    integer :: val
    character(2) :: str = "hi"

    me = this_image()
    n_images = num_images()

    sync all

    val = me
    call co_sum(val)
    call co_max(val)
    call co_min(val)
    call co_max(str)
    call co_min(str)    
    call co_broadcast(val, 1)

    sync all
end program
