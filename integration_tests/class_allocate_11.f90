! Sourced allocation of a polymorphic object copies the value of the source,
! including its allocatable components: a nonpointer function reference
! (whose result is finalized after the statement) and a pointer.
module class_allocate_11_m
    implicit none
    type :: t
        integer, allocatable :: a(:)
    contains
        final :: finish
    end type
    type, extends(t) :: u
        integer :: k = 0
    end type
contains
    function make() result(r)
        type(t) :: r
        allocate(r%a(3))
        r%a = [11, 22, 33]
    end function
    function make_u() result(r)
        type(u) :: r
        allocate(r%a(2))
        r%a = [7, 8]
        r%k = 5
    end function
    subroutine finish(x)
        type(t), intent(inout) :: x
        if (allocated(x%a)) x%a = -99
    end subroutine
end module

program class_allocate_11
    use class_allocate_11_m
    implicit none
    class(t), allocatable :: c, e
    type(t), allocatable :: d
    type(t), pointer :: p
    type(t), target :: y
    allocate(c, source=make())
    if (.not. allocated(c%a)) error stop 1
    if (any(c%a /= [11, 22, 33])) error stop 2
    allocate(d, source=make())
    if (any(d%a /= [11, 22, 33])) error stop 3
    allocate(e, source=make_u())
    select type (e)
    type is (u)
        if (e%k /= 5) error stop 4
        if (any(e%a /= [7, 8])) error stop 5
    class default
        error stop 6
    end select
    deallocate(c)
    allocate(y%a(2))
    y%a = [5, 6]
    p => y
    allocate(c, source=p)
    y%a = [0, 0]
    if (any(c%a /= [5, 6])) error stop 7
    deallocate(c, d, e)
    print *, "ok"
end program
