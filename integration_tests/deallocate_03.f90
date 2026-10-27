program deallocate_03
    ! DEALLOCATE with STAT= defines the stat-variable: zero on success,
    ! nonzero when an object is not allocated (F2023 9.7.4).
    implicit none
    type :: t
        integer, allocatable :: c(:)
    end type
    real(8), allocatable :: rr(:)
    integer, allocatable :: s
    integer, allocatable :: m(:, :)
    character(len=:), allocatable :: str
    integer, pointer :: p(:)
    integer, pointer :: ps
    type(t), pointer :: tp
    class(t), pointer :: cp
    type(t) :: x
    integer :: ierr
    integer(8) :: ierr8
    character(len=20) :: msg

    allocate(rr(10))
    ierr = 7
    deallocate(rr, stat=ierr)
    if (ierr /= 0) error stop 1
    if (allocated(rr)) error stop 2

    allocate(s, m(2, 3))
    ierr = -3
    deallocate(s, m, stat=ierr)
    if (ierr /= 0) error stop 3
    if (allocated(s) .or. allocated(m)) error stop 4

    allocate(character(len=5) :: str)
    ierr = 7
    deallocate(str, stat=ierr)
    if (ierr /= 0) error stop 5
    if (allocated(str)) error stop 6

    allocate(p(4))
    ierr = 7
    deallocate(p, stat=ierr)
    if (ierr /= 0) error stop 7
    if (associated(p)) error stop 8

    allocate(x%c(3))
    ierr8 = 7
    msg = "unchanged"
    deallocate(x%c, stat=ierr8, errmsg=msg)
    if (ierr8 /= 0) error stop 9
    if (msg /= "unchanged") error stop 10
    if (allocated(x%c)) error stop 11

    ! Deallocating an unallocated object is an error condition.
    ierr = 0
    deallocate(rr, stat=ierr)
    if (ierr == 0) error stop 12

    ierr = 0
    deallocate(s, stat=ierr)
    if (ierr == 0) error stop 13

    ierr8 = 0
    deallocate(x%c, stat=ierr8)
    if (ierr8 == 0) error stop 14

    nullify(p)
    ierr = 0
    deallocate(p, stat=ierr)
    if (ierr == 0) error stop 15

    allocate(rr(2))
    ierr = 0
    deallocate(rr, s, stat=ierr)
    if (ierr == 0) error stop 16
    ! The allocation status of rr is processor dependent after the error.
    if (allocated(rr)) deallocate(rr)

    allocate(rr(2))
    ierr = 7
    deallocate(rr, stat=ierr)
    if (ierr /= 0) error stop 17

    ! Deallocating a disassociated pointer is an error condition.
    nullify(ps, tp, cp)
    ierr = 0
    deallocate(ps, stat=ierr)
    if (ierr == 0) error stop 18
    ierr = 0
    deallocate(tp, stat=ierr)
    if (ierr == 0) error stop 19
    ierr = 0
    deallocate(cp, stat=ierr)
    if (ierr == 0) error stop 20

    allocate(tp, cp)
    ierr = 7
    deallocate(tp, cp, stat=ierr)
    if (ierr /= 0) error stop 21
    if (associated(tp) .or. associated(cp)) error stop 22
    print *, "ok"
end program
