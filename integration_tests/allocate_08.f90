program allocate_08

    implicit none
    call allocate_08_split1()

contains

    subroutine allocate_08_split1()
        implicit none
        character(len=:), allocatable :: array(:)
        integer :: ireturn, imax, i

        ireturn = 5
        imax = 5


        allocate(character(len=imax) :: array(ireturn))
        do i = 1, 3
            array(i) = 'hello'
        end do
        do i = 4, 5
            array(i) = 'hi'
        end do

        do i = 1, 3
            print *, array(i)
            if( array(i) /= 'hello' ) error stop
        end do
        do i = 4, 5
            print *, array(i)
            if( array(i) /= 'hi' ) error stop
        end do
    end subroutine allocate_08_split1

end program allocate_08
