module generic_nopass_02_mod
    implicit none

    type :: t
        integer :: base = 10
        integer :: total = 0
    contains
        procedure :: add_real => t_add_real
        procedure, nopass :: set_count => t_set_count
        ! A subroutine generic mixing a `pass` and a `nopass` specific,
        ! listed in both orders.
        generic :: g => add_real, set_count
        generic :: h => set_count, add_real
    end type

    type, extends(t) :: t2
    end type

    integer :: count = 0

contains

    subroutine t_add_real(self, r)
        class(t), intent(inout) :: self
        real, intent(in) :: r
        self%total = self%total + self%base + int(r)
    end subroutine

    subroutine t_set_count(n)
        integer, intent(in) :: n
        count = n
    end subroutine

end module generic_nopass_02_mod

program generic_nopass_02
    use generic_nopass_02_mod
    implicit none
    type(t) :: v
    type(t2) :: w
    class(t), allocatable :: c

    call v%g(1.0)
    if (v%total /= 11) error stop 1
    call v%g(2)
    if (count /= 2) error stop 2

    call v%h(3.0)
    if (v%total /= 24) error stop 3
    call v%h(4)
    if (count /= 4) error stop 4

    call v%g(r=5.0)
    if (v%total /= 39) error stop 5
    call v%h(n=6)
    if (count /= 6) error stop 6

    w%base = 100
    call w%g(1.0)
    if (w%total /= 101) error stop 7
    call w%h(7)
    if (count /= 7) error stop 8

    allocate(t2 :: c)
    c%base = 1000
    call c%h(2.0)
    if (c%total /= 1002) error stop 9
    call c%g(8)
    if (count /= 8) error stop 10

    print *, "ok"
end program generic_nopass_02
