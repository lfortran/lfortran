! A parameter whose constructor passes `null()` to pointer components,
! including a derived-type pointer, initializes entities in other modules,
! whether its type is imported under its own name or renamed.
module structure_constructor_args_07_a
    implicit none
    type :: inner_t
        integer :: k = 0
    end type
    type :: outer_t
        integer :: x
        integer, pointer :: p => null()
        type(inner_t), pointer :: q => null()
    end type
    type(outer_t), parameter :: z = outer_t(5, null(), null())
end module

module structure_constructor_args_07_b
    use structure_constructor_args_07_a, only: u_t => outer_t, z
    implicit none
    type(u_t) :: mv = z
    type :: holder_t
        type(u_t) :: h = z
    end type
end module

module structure_constructor_args_07_c
    use structure_constructor_args_07_a, only: outer_t, z
    implicit none
    type(outer_t) :: cv = z
contains
    integer function local_x()
        type(outer_t) :: y = z
        local_x = y%x
        if (associated(y%p) .or. associated(y%q)) local_x = -1
    end function
end module

program structure_constructor_args_07
    use structure_constructor_args_07_b
    use structure_constructor_args_07_c
    implicit none
    type(holder_t) :: hh
    if (mv%x /= 5) error stop 1
    if (associated(mv%p) .or. associated(mv%q)) error stop 2
    if (hh%h%x /= 5) error stop 3
    if (associated(hh%h%p) .or. associated(hh%h%q)) error stop 4
    if (cv%x /= 5) error stop 5
    if (associated(cv%p) .or. associated(cv%q)) error stop 6
    if (local_x() /= 5) error stop 7
    print *, "ok"
end program
