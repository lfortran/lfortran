module class_pointer_assoc_leak_01_mod
    implicit none
    type :: item_t
        integer :: v = 0
    end type
    type, extends(item_t) :: sub_t
        integer :: w = 0
    end type
    type :: holder_t
        class(item_t), pointer :: c(:) => null()
    end type
    class(item_t), allocatable, target :: g(:)
    class(item_t), pointer :: gp(:)
contains
    subroutine reset(p)
        class(item_t), pointer :: p(:)
        nullify(p)
    end subroutine

    subroutine reset2(p)
        class(item_t), pointer :: p(:, :)
        nullify(p)
    end subroutine
    subroutine assoc_inout(p)
        class(item_t), pointer, intent(inout) :: p(:)
        p => g
    end subroutine

    subroutine assoc_out(p)
        class(item_t), pointer, intent(out) :: p(:)
        p => g
    end subroutine

    subroutine alloc_out(p)
        class(item_t), pointer, intent(out) :: p(:)
        allocate(sub_t :: p(4))
        p%v = 1
    end subroutine

    function get() result(r)
        class(item_t), pointer :: r(:)
        r => g
    end function

    integer function total(x)
        class(item_t), intent(in) :: x(:)
        total = sum(x%v)
    end function

    integer function total_any(input)
        class(*), intent(in) :: input(..)
        total_any = -1
        select rank (input)
        rank (1)
            select type (input)
            class is (item_t)
                total_any = sum(input%v)
            end select
        end select
    end function

    logical function is_sub(x)
        class(item_t), intent(in) :: x(:)
        select type (x)
        type is (sub_t)
            is_sub = .true.
        class default
            is_sub = .false.
        end select
    end function

    subroutine local_pointers()
        class(item_t), allocatable, target :: a(:), b(:)
        type(item_t), target :: t(3)
        class(item_t), pointer :: q(:), p(:), r(:, :)
        type(holder_t) :: h, h2

        allocate(item_t :: a(4))
        a%v = [1, 2, 3, 4]
        allocate(sub_t :: b(2))
        b%v = 5
        t%v = 7

        q => a
        if (total(q) /= 10) error stop
        if (is_sub(q)) error stop
        q => b
        if (total(q) /= 10) error stop
        if (.not. is_sub(q)) error stop

        p => q
        if (total(p) /= 10) error stop
        if (.not. is_sub(p)) error stop

        nullify(q)
        if (associated(q)) error stop
        q => a
        if (total(q) /= 10) error stop

        q => t
        if (total(q) /= 21) error stop
        if (is_sub(q)) error stop

        q => a(2:3)
        if (total(q) /= 5) error stop

        r(1:2, 1:2) => a
        if (size(r) /= 4) error stop

        call assoc_inout(q)
        if (total(q) /= 6) error stop
        call assoc_out(p)
        if (total(p) /= 6) error stop
        q => get()
        if (total(q) /= 6) error stop
        if (total(get()) /= 6) error stop

        nullify(p)
        call alloc_out(p)
        if (total(p) /= 4) error stop
        if (.not. is_sub(p)) error stop
        deallocate(p)

        allocate(item_t :: p(2))
        p%v = 3
        if (total(p) /= 6) error stop
        deallocate(p)
        nullify(p)

        h%c => a
        if (total(h%c) /= 10) error stop
        h2 = h
        if (total(h2%c) /= 10) error stop
        h%c => b
        if (total(h%c) /= 10) error stop
        if (total(h2%c) /= 10) error stop
        nullify(h%c)
    end subroutine

    ! The wrapper of a save pointer is freed through a pointer dummy, too.
    subroutine save_pointers()
        class(item_t), pointer :: q(:) => null()
        class(item_t), pointer, save :: r(:, :)

        q => g(1:2)
        if (total(q) /= 4) error stop
        call reset(q)
        if (associated(q)) error stop

        r(1:3, 1:1) => g
        if (size(r) /= 3) error stop
        call reset2(r)
        if (associated(r)) error stop

        gp => g
        if (total(gp) /= 6) error stop
        nullify(gp)
        gp => g(2:3)
        if (total(gp) /= 4) error stop
        call reset(gp)
        if (associated(gp)) error stop
    end subroutine

    ! Disassociation frees the wrapper of class(t) and class(*) pointers.
    subroutine disassociate()
        integer, target :: ia(3)
        class(item_t), pointer :: q(:)
        class(*), pointer :: u(:)
        ia = 2

        q => g
        q => null()
        if (associated(q)) error stop

        u => ia
        if (size(u) /= 3) error stop
        u => null()
        if (associated(u)) error stop
        u => ia
        nullify(u)
        if (associated(u)) error stop
        u => g
        if (size(u) /= 3) error stop
        nullify(u)
        if (associated(u)) error stop
    end subroutine
end module

program class_pointer_assoc_leak_01
    use class_pointer_assoc_leak_01_mod
    implicit none
    type(item_t) :: t(3)
    integer :: i
    allocate(item_t :: g(3))
    g%v = 2
    t%v = 4
    do i = 1, 3
        call local_pointers()
        call save_pointers()
        call disassociate()
        block
            class(item_t), pointer :: bq(:)
            bq => g
            if (total(bq) /= 6) error stop
        end block
        if (total_any(t) /= 12) error stop
    end do
    print *, "ok"
end program
