module allocatable_polymorphic_source_01_m
    implicit none
    type :: base
        integer :: i = 0
    end type
    type, extends(base) :: ext_int
        integer :: c = 0
    end type
    type, extends(base) :: ext_chars
        character(len=2) :: c(3)
    end type
    type, extends(base) :: ext_char
        character(len=2) :: c
        integer, allocatable :: d(:)
    end type
end module

program allocatable_polymorphic_source_01
    ! lfortran/lfortran#13598: allocate(y, source=x) with a polymorphic
    ! array x gives y the dynamic type and the values of x.
    use allocatable_polymorphic_source_01_m
    implicit none
    call copy_int()
    call copy_chars()
    call copy_char()
    call copy_mixed()
    print *, "ok"

contains

    subroutine copy_int()
        class(base), allocatable :: x(:), y(:)
        allocate(ext_int :: x(2))
        select type (x)
        type is (ext_int)
            x(2)%c = 5
        end select
        allocate(y, source=x)
        if (size(y) /= 2) error stop 1
        select type (y)
        type is (ext_int)
            if (y(1)%c /= 0) error stop 2
            if (y(2)%c /= 5) error stop 3
        class default
            error stop 4
        end select
    end subroutine

    subroutine copy_chars()
        class(base), allocatable :: x(:), y(:)
        allocate(ext_chars :: x(2))
        select type (x)
        type is (ext_chars)
            x(2)%c = ['ab', 'cd', 'ef']
        end select
        allocate(y, source=x)
        if (size(y) /= 2) error stop 11
        select type (y)
        type is (ext_chars)
            if (any(y(2)%c /= ['ab', 'cd', 'ef'])) error stop 12
        class default
            error stop 13
        end select
    end subroutine

    subroutine copy_char()
        class(base), allocatable :: x(:), y(:)
        allocate(ext_char :: x(2))
        select type (x)
        type is (ext_char)
            x(1)%c = 'gh'
            x(2)%c = 'ef'
            allocate(x(2)%d(3))
            x(2)%d = [1, 2, 3]
        end select
        allocate(y, source=x)
        if (size(y) /= 2) error stop 21
        select type (y)
        type is (ext_char)
            if (y(1)%c /= 'gh') error stop 22
            if (y(2)%c /= 'ef') error stop 23
            if (allocated(y(1)%d)) error stop 24
            if (.not. allocated(y(2)%d)) error stop 25
            if (any(y(2)%d /= [1, 2, 3])) error stop 26
        class default
            error stop 27
        end select
        select type (x)
        type is (ext_char)
            x(2)%d(1) = 7
        end select
        select type (y)
        type is (ext_char)
            if (y(2)%d(1) /= 1) error stop 28
        end select
    end subroutine

    subroutine copy_mixed()
        ! A polymorphic and a nonpolymorphic object in one ALLOCATE, in
        ! both orders.
        class(base), allocatable :: x(:), y(:)
        type(base), allocatable :: z(:)
        integer :: k
        allocate(ext_int :: x(3))
        x%i = [1, 2, 3]
        select type (x)
        type is (ext_int)
            x%c = [4, 5, 6]
        end select
        do k = 1, 2
            if (k == 1) then
                allocate(y, z, source=x)
            else
                allocate(z, y, source=x)
            end if
            if (size(y) /= 3) error stop 30 + 10*k
            if (size(z) /= 3) error stop 31 + 10*k
            if (any(z%i /= [1, 2, 3])) error stop 32 + 10*k
            if (any(y%i /= [1, 2, 3])) error stop 33 + 10*k
            select type (y)
            type is (ext_int)
                if (any(y%c /= [4, 5, 6])) error stop 34 + 10*k
            class default
                error stop 35 + 10*k
            end select
            deallocate(y, z)
        end do
        ! STAT= reports the failure to allocate the already allocated z.
        allocate(z(3))
        k = 0
        allocate(z(3), y(3), source=x, stat=k)
        if (k == 0) error stop 50
    end subroutine
end program
