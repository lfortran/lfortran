! A local allocatable or pointer array with the SAVE attribute keeps its
! allocation status, bounds and association between calls.
module save_28_mod
    implicit none
    type :: item_t
        integer :: a = 5
        real, allocatable :: r(:)
    end type
contains
    integer function first_use(k) result(n_alloc)
        integer, intent(in) :: k
        real, allocatable, save :: z(:)
        integer, save :: count = 0
        if (.not. allocated(z)) then
            count = count + 1
            allocate(z(3))
            z = 0.0
        end if
        z(1) = z(1) + real(k)
        n_alloc = count
        if (abs(z(1) - real(k*(k+1)/2)) > 1e-6) error stop
    end function

    subroutine with_flag(k)
        integer, intent(in) :: k
        real(8), allocatable, save :: w(:,:,:,:)
        integer, save :: ifirst = 0
        if (ifirst == 0) then
            ifirst = 1
            allocate(w(2,2,2,2))
            w = 0.0d0
        end if
        w(1,1,1,1) = w(1,1,1,1) + 1.0d0
        if (abs(w(1,1,1,1) - real(k, 8)) > 1d-12) error stop
    end subroutine

    subroutine save_statement(k)
        integer, intent(in) :: k
        integer, allocatable :: b(:,:)
        save :: b
        if (k == 1) then
            if (allocated(b)) error stop
            allocate(b(-1:1, 2:5))
            b = 7
        else if (k == 2) then
            if (.not. allocated(b)) error stop
            if (any(lbound(b) /= [-1, 2])) error stop
            if (any(ubound(b) /= [1, 5])) error stop
            if (any(b /= 7)) error stop
            deallocate(b)
            allocate(b(0:2, 1:4))
            b = 8
        else
            if (.not. allocated(b)) error stop
            if (any(lbound(b) /= [0, 1])) error stop
            if (any(ubound(b) /= [2, 4])) error stop
            if (any(b /= 8)) error stop
        end if
    end subroutine

    subroutine saved_strings(k)
        integer, intent(in) :: k
        character(len=:), allocatable, save :: c(:)
        character(len=4), allocatable, save :: d(:)
        if (k == 1) then
            if (allocated(c) .or. allocated(d)) error stop
            allocate(character(len=3) :: c(2))
            c = 'abc'
            allocate(d(2))
            d = 'wxyz'
        end if
        if (.not. allocated(c) .or. .not. allocated(d)) error stop
        if (len(c) /= 3 .or. any(c /= 'abc')) error stop
        if (len(d) /= 4 .or. any(d /= 'wxyz')) error stop
    end subroutine

    subroutine saved_items(k)
        integer, intent(in) :: k
        type(item_t), allocatable, save :: x(:)
        if (k == 1) then
            if (allocated(x)) error stop
            allocate(x(2))
            x(2)%a = 9
            allocate(x(2)%r(3))
            x(2)%r = 2.5
        end if
        if (.not. allocated(x)) error stop
        if (x(1)%a /= 5 .or. x(2)%a /= 9) error stop
        if (.not. allocated(x(2)%r)) error stop
        if (any(x(2)%r /= 2.5)) error stop
    end subroutine

    subroutine saved_pointer(k)
        integer, intent(in) :: k
        integer, pointer, save :: p(:) => null()
        if (k == 1) then
            if (associated(p)) error stop
            allocate(p(2))
            p = [3, 4]
        end if
        if (.not. associated(p)) error stop
        if (any(p /= [3, 4])) error stop
        if (k == 3) then
            deallocate(p)
        end if
    end subroutine
end module

program save_28
    use save_28_mod
    implicit none
    integer :: k, n
    do k = 1, 3
        n = first_use(k)
        if (n /= 1) error stop
        call with_flag(k)
        call save_statement(k)
        call saved_strings(k)
        call saved_items(k)
        call saved_pointer(k)
    end do
    call contained(1)
    call contained(2)
    print *, "ok"
contains
    subroutine contained(k)
        integer, intent(in) :: k
        logical, allocatable, save :: f(:)
        if (k == 1) then
            if (allocated(f)) error stop
            allocate(f(4))
            f = .true.
        end if
        if (.not. allocated(f)) error stop
        if (.not. all(f)) error stop
    end subroutine
end program
