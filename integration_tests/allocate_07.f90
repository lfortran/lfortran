program allocate_07

    implicit none
    call allocate_07_split1()

contains

    subroutine allocate_07_split1()
        implicit none
        character(len=:),allocatable :: array(:)

        integer :: ireturn, imax, i
        ireturn = 5
        imax = 5

        allocate(character(len=imax) :: array(ireturn))
        do i=1,3
            array(i)=' '
        end do
        print *, "working allocate_07_split1"
    end subroutine allocate_07_split1

end program allocate_07
