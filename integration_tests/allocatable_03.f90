program allocatable_03
    implicit none
    character(:), allocatable :: s
    integer, allocatable :: n
    allocate(character(0) :: s)
    call inspect(s)
    call fill(s)
    if (.not. allocated(s)) error stop 1
    if (len(s) /= 2) error stop 2
    if (s /= "ok") error stop 3
    call release(s)
    if (allocated(s)) error stop 4
    call set_integer(n)
    if (.not. allocated(n)) error stop 5
    if (n /= 7) error stop 6
    call clear_integer(n)
    if (allocated(n)) error stop 7
    s = result_string(.true.)
    if (s /= "yes") error stop 8
    s = result_string(.false.)
    if (s /= "no") error stop 9
contains
    function result_string(early) result(t)
        logical, intent(in) :: early
        character(:), allocatable :: t
        t = "yes"
        if (early) return
        t = "no"
    end function
    subroutine inspect(t)
        character(:), allocatable, intent(in) :: t
        if (.not. allocated(t)) error stop 11
        if (len(t) /= 0) error stop 12
    end subroutine
    subroutine fill(t)
        character(:), allocatable, intent(out) :: t
        if (allocated(t)) error stop 21
        t = "ok"
    end subroutine
    subroutine release(t)
        character(:), allocatable, intent(inout) :: t
        if (.not. allocated(t)) error stop 31
        deallocate(t)
    end subroutine
    subroutine set_integer(t)
        integer, allocatable, intent(out) :: t
        if (allocated(t)) error stop 41
        t = 7
    end subroutine
    subroutine clear_integer(t)
        integer, allocatable, intent(inout) :: t
        if (.not. allocated(t)) error stop 51
        deallocate(t)
    end subroutine
end program
