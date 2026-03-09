program random_init_01
    implicit none

    ! Test random_init with keyword arguments (image_distinct)
    call random_init(repeatable=.true., image_distinct=.true.)
    call random_init(image_distinct=.false., repeatable=.false.)
    call random_init(.true., .false.)

    print *, "ok"
end program
