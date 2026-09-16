! A parallel do and a do concurrent call a procedure contained in the program,
! which is copied out of the program with the loop. Inside a BLOCK, an
! ASSOCIATE and a SELECT TYPE of the copy, it calls a procedure declared in
! the copy itself: an external procedure of an interface block, and a module
! procedure of a USE statement of the copy.
module implicit_interface_107_m
    implicit none
contains
    pure integer function triple(k)
        integer, intent(in) :: k
        triple = 3*k
    end function
end module

program implicit_interface_107
    implicit none
    integer :: i, b(4), c(4)
    !$omp parallel do
    do i = 1, 4
        b(i) = combined(i)
    end do
    !$omp end parallel do
    if (b(1) /= 25 .or. b(2) /= 50 .or. b(3) /= 75 .or. b(4) /= 100) error stop 1
    do concurrent (i = 1:4)
        c(i) = combined(i)
    end do
    if (c(1) /= 25 .or. c(2) /= 50 .or. c(3) /= 75 .or. c(4) /= 100) error stop 2
    print *, b, c
contains
    pure integer function combined(k)
        use implicit_interface_107_m, only: triple
        integer, intent(in) :: k
        interface
            pure subroutine implicit_interface_107_ext(k, r)
                integer, intent(in) :: k
                integer, intent(out) :: r
            end subroutine
            pure integer function implicit_interface_107_fext(k)
                integer, intent(in) :: k
            end function
        end interface
        class(*), allocatable :: v
        integer :: r
        block
            call implicit_interface_107_ext(k, r)
        end block
        associate (s => triple(k) + implicit_interface_107_fext(k))
            r = r + s
        end associate
        v = k
        select type (v)
        type is (integer)
            r = r + implicit_interface_107_fext(v) + triple(v)
        end select
        combined = r
    end function
end program

pure subroutine implicit_interface_107_ext(k, r)
    integer, intent(in) :: k
    integer, intent(out) :: r
    r = 5*k
end subroutine

pure integer function implicit_interface_107_fext(k)
    integer, intent(in) :: k
    implicit_interface_107_fext = 7*k
end function
