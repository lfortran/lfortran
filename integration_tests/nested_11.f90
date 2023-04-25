module base
implicit none

    integer, public :: a = 10

end module

module intermediate
use base, only: a
implicit none
private
end module

program nested_11
use base
implicit none

    interface
        subroutine sub2
          use intermediate
          implicit none
        end subroutine sub2
    end interface

    call sub

contains

    subroutine sub
        use intermediate
        implicit none
        print *, a
        if( a /= 10 ) error stop
    end subroutine

end program
