! move_alloc transfers the allocation of a scalar of a finalizable type:
! neither the transferred object nor the unallocated destination is
! finalized, and pointers associated with the source stay associated
! with the destination
module move_alloc_finalize_01_mod
    implicit none
    integer :: nfin = 0

    type :: t
        integer :: x = 0
        integer, pointer :: p => null()
    contains
        final :: finish
    end type

    type, extends(t) :: u
        integer :: y = 0
    end type

contains

    subroutine finish(self)
        type(t), intent(inout) :: self
        nfin = nfin + 1
        if (associated(self%p)) deallocate(self%p)
    end subroutine

end module

program move_alloc_finalize_01
    use move_alloc_finalize_01_mod
    implicit none
    type(t), allocatable, target :: a, b
    type(t), pointer :: q
    class(t), allocatable :: c, d
    class(*), allocatable :: e
    integer, allocatable, target :: ia, ib
    integer, pointer :: iq

    allocate(a)
    a%x = 4
    allocate(a%p, source=42)
    q => a
    call move_alloc(a, b)
    if (nfin /= 0) error stop 1
    if (allocated(a)) error stop 2
    if (.not. allocated(b)) error stop 3
    if (b%x /= 4) error stop 4
    if (.not. associated(b%p)) error stop 5
    if (b%p /= 42) error stop 6
    q%x = 9
    if (b%x /= 9) error stop 7

    ! An allocated destination is deallocated, and thus finalized, once
    allocate(a)
    a%x = 5
    call move_alloc(a, b)
    if (nfin /= 1) error stop 8
    if (allocated(a)) error stop 9
    if (b%x /= 5) error stop 10
    if (associated(b%p)) error stop 11

    ! An unallocated source leaves the destination unallocated
    call move_alloc(a, b)
    if (nfin /= 2) error stop 12
    if (allocated(b)) error stop 13

    ! Polymorphic destinations keep the dynamic type
    allocate(u :: c)
    c%x = 6
    call move_alloc(c, d)
    if (nfin /= 2) error stop 14
    if (allocated(c)) error stop 15
    select type (d)
    type is (u)
        if (d%x /= 6) error stop 16
    class default
        error stop 17
    end select
    deallocate(d)
    if (nfin /= 3) error stop 18

    allocate(a)
    a%x = 7
    allocate(a%p, source=43)
    call move_alloc(a, d)
    if (nfin /= 3) error stop 19
    if (allocated(a)) error stop 20
    select type (d)
    type is (t)
        if (d%x /= 7) error stop 21
        if (d%p /= 43) error stop 22
    class default
        error stop 23
    end select
    deallocate(d)
    if (nfin /= 4) error stop 24

    allocate(ia, source=7)
    iq => ia
    call move_alloc(ia, ib)
    if (allocated(ia)) error stop 25
    if (ib /= 7) error stop 26
    iq = 8
    if (ib /= 8) error stop 27

    call move_alloc(ib, e)
    if (allocated(ib)) error stop 28
    select type (e)
    type is (integer)
        if (e /= 8) error stop 29
    class default
        error stop 30
    end select
    print *, "ok"
end program
