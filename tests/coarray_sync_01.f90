program coarray_sync_01
    implicit none
    integer :: images(2)

    sync all
    sync memory
    
    images(1) = 1
    images(2) = 2
    sync images(images)
    
    sync images(*)
end program
