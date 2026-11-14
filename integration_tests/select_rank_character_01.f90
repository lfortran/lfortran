! a character scalar passed to a character assumed-rank dummy (plain,
! pointer or allocatable) has rank 0 and its value
module select_rank_character_01_m
    implicit none
contains
    integer function check_ptr(x) result(r)
        character(len=3), pointer, intent(in) :: x(..)
        r = -1
        select rank (x)
        rank (0)
            if (len(x) /= 3) error stop 1
            if (x /= "abc") error stop 2
            r = 0
        rank (1)
            if (size(x) /= 2) error stop 3
            if (x(1) /= "abc" .or. x(2) /= "def") error stop 4
            r = 1
        end select
    end function

    integer function check_deferred_ptr(x) result(r)
        character(len=:), pointer, intent(in) :: x(..)
        r = -1
        select rank (x)
        rank (0)
            if (len(x) /= 5) error stop 5
            if (x /= "hello") error stop 6
            r = 0
        end select
    end function

    integer function check_alloc(x) result(r)
        character(len=3), allocatable, intent(inout) :: x(..)
        r = -1
        select rank (x)
        rank (0)
            if (x /= "abc") error stop 8
            r = 0
        end select
    end function

    integer function check_deferred_alloc(x) result(r)
        character(len=:), allocatable, intent(inout) :: x(..)
        r = -1
        select rank (x)
        rank (0)
            if (len(x) /= 4) error stop 9
            if (x /= "abcd") error stop 15
            r = 0
        end select
    end function

    integer function check_plain(x) result(r)
        character(len=3), intent(in) :: x(..)
        r = -1
        select rank (x)
        rank (0)
            if (x /= "xyz") error stop 7
            r = 0
        end select
    end function
end module

program select_rank_character_01
    use select_rank_character_01_m
    implicit none
    character(len=3), target :: c, a(2)
    character(len=3), pointer :: p, pa(:)
    character(len=5), target :: h
    character(len=:), pointer :: q
    character(len=3), allocatable :: ca
    character(len=:), allocatable :: da
    c = "abc"
    p => c
    if (check_ptr(p) /= 0) error stop 10
    a = ["abc", "def"]
    pa => a
    if (check_ptr(pa) /= 1) error stop 11
    h = "hello"
    q => h
    if (check_deferred_ptr(q) /= 0) error stop 12
    allocate(ca)
    ca = "abc"
    if (check_alloc(ca) /= 0) error stop 16
    da = "abcd"
    if (check_deferred_alloc(da) /= 0) error stop 17
    c = "xyz"
    if (check_plain(c) /= 0) error stop 13
    if (check_plain(p) /= 0) error stop 14
    print *, "ok"
end program
