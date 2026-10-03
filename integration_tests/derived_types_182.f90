module derived_types_182_mod
    implicit none

    type :: t
        character(len=3) :: a(4) = ["aaa", "bbb", "ccc", "ddd"]
    end type t

    type :: t_alloc
        character(len=3), allocatable :: a(:)
    end type t_alloc

    type :: t_deferred
        character(len=:), allocatable :: a(:)
    end type t_deferred

    type :: t_ptr
        character(len=3), pointer :: pc(:) => null()
    end type t_ptr

    type :: base
        character(len=3) :: bc(4) = ["aaa", "bbb", "ccc", "ddd"]
    end type base

    type :: holder
        type(base) :: b
    end type holder

    type, extends(base) :: der
        integer :: n = 0
    end type der

contains

    subroutine check_section(x, e1, e2)
        character(len=*), intent(in) :: x(:)
        character(len=*), intent(in) :: e1, e2
        if (size(x) /= 2) error stop "check_section: wrong size"
        if (x(1) /= e1) error stop "check_section: wrong first element"
        if (x(2) /= e2) error stop "check_section: wrong second element"
    end subroutine check_section

end module derived_types_182_mod

program derived_types_182
    use derived_types_182_mod
    implicit none

    type(t) :: v
    type(t) :: vs(3)
    type(t_alloc) :: va
    type(t_deferred) :: vd
    type(t_ptr) :: vp
    type(holder) :: h
    type(der) :: d
    character(len=3), target :: tgt(4) = ["qqq", "rrr", "sss", "ttt"]
    character(len=3) :: c(2)

    ! The whole component and a single element already worked; keep them covered.
    if (size(v%a) /= 4) error stop "whole component: wrong size"
    if (v%a(1) // v%a(2) // v%a(3) // v%a(4) /= "aaabbbcccddd") error stop "whole component"
    if (v%a(2) /= "bbb") error stop "single element"

    ! Printing a section of a character array component.
    print *, v%a(2:3)

    ! Assigning a section of a character array component.
    c = v%a(2:3)
    if (c(1) /= "bbb") error stop "section assignment: first element"
    if (c(2) /= "ccc") error stop "section assignment: second element"

    ! Strided section.
    c = v%a(1:4:2)
    if (c(1) /= "aaa") error stop "strided section: first element"
    if (c(2) /= "ccc") error stop "strided section: second element"

    ! Section passed as an actual argument to character(len=*) :: x(:).
    call check_section(v%a(2:3), "bbb", "ccc")
    call check_section(v%a(1:4:2), "aaa", "ccc")

    ! Section of a component of an array of the derived type.
    vs(2)%a = ["eee", "fff", "ggg", "hhh"]
    c = vs(2)%a(2:3)
    if (c(1) /= "fff") error stop "array element component section: first element"
    if (c(2) /= "ggg") error stop "array element component section: second element"
    call check_section(vs(2)%a(2:3), "fff", "ggg")

    ! Allocatable character array component.
    allocate(va%a(4))
    va%a = ["iii", "jjj", "kkk", "lll"]
    c = va%a(2:3)
    if (c(1) /= "jjj") error stop "allocatable component section: first element"
    if (c(2) /= "kkk") error stop "allocatable component section: second element"
    call check_section(va%a(2:3), "jjj", "kkk")

    ! Deferred length allocatable character array component.
    allocate(character(len=3) :: vd%a(4))
    vd%a = ["mmm", "nnn", "ooo", "ppp"]
    c = vd%a(2:3)
    if (c(1) /= "nnn") error stop "deferred length component section: first element"
    if (c(2) /= "ooo") error stop "deferred length component section: second element"

    ! Pointer character array component: the field does hold the address of a
    ! descriptor, so this pins the pointer half of the guard.
    vp%pc => tgt
    c = vp%pc(2:3)
    if (c(1) /= "rrr") error stop "pointer component section: first element"
    if (c(2) /= "sss") error stop "pointer component section: second element"
    call check_section(vp%pc(2:3), "rrr", "sss")

    ! Section of a character array component of a nested derived type.
    c = h%b%bc(2:3)
    if (c(1) /= "bbb") error stop "nested component section: first element"
    if (c(2) /= "ccc") error stop "nested component section: second element"
    call check_section(h%b%bc(2:3), "bbb", "ccc")
    h%b%bc = ["eee", "fff", "ggg", "hhh"]
    c = h%b%bc(2:3)
    if (c(1) /= "fff") error stop "nested component section after assignment: first element"
    if (c(2) /= "ggg") error stop "nested component section after assignment: second element"

    ! Section of a character array component inherited from a parent type.
    c = d%bc(2:3)
    if (c(1) /= "bbb") error stop "inherited component section: first element"
    if (c(2) /= "ccc") error stop "inherited component section: second element"
    call check_section(d%bc(2:3), "bbb", "ccc")
    d%bc = ["iii", "jjj", "kkk", "lll"]
    c = d%bc(2:3)
    if (c(1) /= "jjj") error stop "inherited component section after assignment: first element"
    if (c(2) /= "kkk") error stop "inherited component section after assignment: second element"

    print *, "ok"

end program derived_types_182
