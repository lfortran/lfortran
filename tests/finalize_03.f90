program finalize_03
    type t1
        integer :: arr2(1)
        character(len=1) :: name
    end type

    type t2
        type(t1) :: arr(1)
    end type t2

    type parent
        type(t2) ,allocatable :: z(:)
    end type

    type, extends(parent) ::child
        type(t1), allocatable :: lol
        integer, allocatable :: arr(:)
        character(:), allocatable :: str
    end type

    call ss

    contains
    subroutine ss
        type(child) :: c(1)
        allocate(c(1)%z(1))
        allocate(c(1)%lol)
        allocate(c(1)%arr(1))
        allocate(character(len=10) :: c(1)%str)
    end subroutine
end program
