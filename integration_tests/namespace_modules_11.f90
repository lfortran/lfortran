! A module entity is accessible by host association: in internal procedures,
! in module procedures of the module that imported it, and in BLOCK
! constructs. A local entity with the same name in an inner scope hides it.
module namespace_modules_11_lib
    implicit none
    integer :: hits = 0
contains
    integer function add1(i)
        integer, intent(in) :: i
        add1 = i + 1
        hits = hits + 1
    end function
end module

module namespace_modules_11_client
    use, namespace :: lib => namespace_modules_11_lib
    implicit none
contains
    integer function add2(i)
        integer, intent(in) :: i
        add2 = lib%add1(lib%add1(i))
    end function
end module

program namespace_modules_11
    use, namespace :: lib => namespace_modules_11_lib
    use namespace_modules_11_client, only: add2
    implicit none

    if (add2(1) /= 3) error stop
    if (lib%hits /= 2) error stop
    call internal()
    if (lib%hits /= 3) error stop
    if (internal_fn(5) /= 6) error stop
    if (lib%hits /= 4) error stop

    block
        integer :: k
        k = lib%add1(10)
        if (k /= 11) error stop
    end block
    if (lib%hits /= 5) error stop

    call shadow()
    if (lib%hits /= 5) error stop
    print *, lib%hits
contains
    subroutine internal()
        if (lib%add1(0) /= 1) error stop
    end subroutine

    integer function internal_fn(i)
        integer, intent(in) :: i
        internal_fn = lib%add1(i)
    end function

    subroutine shadow()
        ! This local variable hides the host's namespace "lib"
        integer :: lib
        lib = 42
        if (lib /= 42) error stop
    end subroutine
end program
