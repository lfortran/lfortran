! Allocatable and pointer non-polymorphic actual arguments passed to a
! class(*) dummy must carry their dynamic type into select type
module select_type_55_mod
    implicit none
    type :: item_t
        integer :: v = 0
    end type
    type :: holder_t
        type(item_t), allocatable :: it
    end type
contains
    integer function get(item) result(r)
        class(*) :: item
        r = -1
        select type (item)
        type is (item_t)
            r = item%v
        type is (integer)
            r = item
        end select
    end function

    subroutine consumes(r, item)
        integer, intent(out) :: r
        class(*) :: item
        r = -1
        select type (item)
        type is (item_t)
            r = item%v
        end select
    end subroutine
end module

program select_type_55
    use select_type_55_mod
    implicit none
    type(item_t), allocatable :: a
    type(item_t), pointer :: pp
    type(item_t), target :: t
    type(holder_t) :: h
    integer, allocatable :: ia
    integer, pointer :: ip
    integer, target :: it
    integer :: r

    allocate(a)
    a%v = 4
    t%v = 5
    pp => t
    allocate(h%it)
    h%it%v = 8
    allocate(ia)
    ia = 6
    it = 7
    ip => it

    call consumes(r, a)
    if (r /= 4) error stop 1
    if (get(a) /= 4) error stop 2
    if (get(pp) /= 5) error stop 3
    if (get(h%it) /= 8) error stop 4
    if (get(ia) /= 6) error stop 5
    if (get(ip) /= 7) error stop 6
    print *, "ok"
end program
