program class_110
    use class_110_module
    implicit none

    type(wrapper) :: w

    allocate(base_type :: w%obj)
    w%obj%k = 4

    call w%caller()

    print *, "ok"
end program class_110
