program constant_fold
    implicit none

    do while (.false.)
        print *, "This will never be printed"
    end do

    print *, "Loop skipped. Program done."
end program constant_fold
