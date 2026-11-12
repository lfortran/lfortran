module class_intent_out_dynamic_01_m
    implicit none
    type :: base
        integer :: root_id = -1
    end type
    type, extends(base) :: derived
        logical :: flag = .false.
        integer :: counter = 0
        real :: r(2) = [1.0, 2.0]
        character(len=3) :: s = "abc"
        integer, allocatable :: buf(:)
    end type
contains
    subroutine reset(this)
        class(base), intent(out) :: this
    end subroutine
end module

program class_intent_out_dynamic_01
    use class_intent_out_dynamic_01_m
    implicit none
    type(derived) :: d
    class(base), allocatable :: p
    d%root_id = 5
    d%flag = .true.
    d%counter = 7
    d%r = [9.0, 9.0]
    d%s = "xyz"
    allocate(d%buf(3))
    call reset(d)
    if (d%root_id /= -1) error stop 1
    if (d%flag) error stop 2
    if (d%counter /= 0) error stop 3
    if (any(d%r /= [1.0, 2.0])) error stop 4
    if (d%s /= "abc") error stop 5
    if (allocated(d%buf)) error stop 6

    allocate(derived :: p)
    select type (p)
    type is (derived)
        p%counter = 3
        p%flag = .true.
    end select
    call reset(p)
    select type (p)
    type is (derived)
        if (p%counter /= 0) error stop 7
        if (p%flag) error stop 8
    end select

end program
