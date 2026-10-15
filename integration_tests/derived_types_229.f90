! A derived type declared in a procedure extends a type accessible by host
! association: from the containing program, the containing module, a module
! used by the containing module, and a module used by the containing program.
module derived_types_229_base
implicit none
type :: base_t
    integer :: a1 = 1
end type
contains
integer function get_a1(x) result(r)
    class(base_t), intent(in) :: x
    r = x%a1
end function
end module

module derived_types_229_m
implicit none
type :: mod_base_t
    integer :: b1 = 3
end type
contains
subroutine from_module()
    type, extends(mod_base_t) :: loc_t
        integer :: z = 4
    end type
    type(loc_t) :: v
    if (v%b1 /= 3 .or. v%z /= 4) error stop
    v%b1 = 30
    if (v%mod_base_t%b1 /= 30) error stop
end subroutine
end module

module derived_types_229_use
use derived_types_229_base
implicit none
contains
subroutine from_used_module()
    type, extends(base_t) :: loc_t
        integer :: z = 6
    end type
    type(loc_t) :: v
    v%a1 = 5
    if (get_a1(v) /= 5 .or. v%z /= 6) error stop
end subroutine
end module

program derived_types_229
use derived_types_229_base
use derived_types_229_m
use derived_types_229_use
implicit none
type :: prog_base_t
    integer :: p1 = 7
end type
call from_module()
call from_used_module()
call from_program()
call from_program_use()
print *, "ok"
contains
subroutine from_program()
    type, extends(prog_base_t) :: loc_t
        integer :: z = 8
    end type
    type(loc_t) :: v
    if (v%p1 /= 7 .or. v%z /= 8) error stop
    v%p1 = 70
    if (v%p1 /= 70) error stop
end subroutine

subroutine from_program_use()
    type, extends(base_t) :: loc_t
        integer :: z = 9
    end type
    class(base_t), allocatable :: c
    allocate(loc_t :: c)
    c%a1 = 11
    select type (c)
    type is (loc_t)
        if (c%z /= 9) error stop
    class default
        error stop
    end select
    if (get_a1(c) /= 11) error stop
end subroutine
end program
