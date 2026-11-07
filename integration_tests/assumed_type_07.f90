! c_loc of a type(*) dummy associated with a character actual argument is
! the address of the actual's character data (F2023 18.2.3.7), and writes
! through that address modify the actual.
module assumed_type_07_mod
    use, intrinsic :: iso_c_binding, only: c_ptr, c_loc, c_f_pointer, &
        c_associated, c_char
    implicit none
contains

    subroutine set_first(p)
        type(c_ptr), intent(in) :: p
        character(kind=c_char), pointer :: cp(:)
        call c_f_pointer(p, cp, [1])
        cp(1) = 'X'
    end subroutine

    logical function scalar_at(buf, q)
        type(*), target :: buf
        type(c_ptr), intent(in) :: q
        scalar_at = c_associated(c_loc(buf), q)
        call set_first(c_loc(buf))
    end function

    logical function rank_at(buf, q)
        type(*), dimension(..), target :: buf
        type(c_ptr), intent(in) :: q
        rank_at = c_associated(c_loc(buf), q)
        call set_first(c_loc(buf))
    end function

    logical function shape_at(buf, q)
        type(*), dimension(:), target :: buf
        type(c_ptr), intent(in) :: q
        shape_at = c_associated(c_loc(buf), q)
        call set_first(c_loc(buf))
    end function

    logical function size_at(buf, q)
        type(*), dimension(*), target :: buf
        type(c_ptr), intent(in) :: q
        size_at = c_associated(c_loc(buf), q)
        call set_first(c_loc(buf))
    end function

    logical function fwd_scalar_at(buf, q)
        type(*), target :: buf
        type(c_ptr), intent(in) :: q
        fwd_scalar_at = scalar_at(buf, q)
    end function

    logical function fwd_rank_at(buf, q)
        type(*), dimension(..), target :: buf
        type(c_ptr), intent(in) :: q
        fwd_rank_at = rank_at(buf, q)
    end function

end module

program assumed_type_07
    use assumed_type_07_mod
    implicit none
    character(len=7), target :: s
    character(len=1), target :: c
    character(len=3), target :: a(4)

    s = 'abcdefg'
    if (.not. scalar_at(s, c_loc(s))) error stop 1
    if (s /= 'Xbcdefg') error stop 2

    c = 'a'
    if (.not. scalar_at(c, c_loc(c))) error stop 3
    if (c /= 'X') error stop 4

    s = 'abcdefg'
    if (.not. rank_at(s, c_loc(s))) error stop 5
    if (s /= 'Xbcdefg') error stop 6

    c = 'a'
    if (.not. rank_at(c, c_loc(c))) error stop 7
    if (c /= 'X') error stop 8

    a = ['abc', 'def', 'ghi', 'jkl']
    if (.not. rank_at(a, c_loc(a))) error stop 9
    if (a(1) /= 'Xbc' .or. a(4) /= 'jkl') error stop 10

    a = ['abc', 'def', 'ghi', 'jkl']
    if (.not. shape_at(a, c_loc(a))) error stop 11
    if (a(1) /= 'Xbc' .or. a(4) /= 'jkl') error stop 12

    a = ['abc', 'def', 'ghi', 'jkl']
    if (.not. size_at(a, c_loc(a))) error stop 13
    if (a(1) /= 'Xbc' .or. a(4) /= 'jkl') error stop 14

    a = ['abc', 'def', 'ghi', 'jkl']
    if (.not. scalar_at(a(2), c_loc(a(2)))) error stop 15
    if (a(2) /= 'Xef' .or. a(1) /= 'abc') error stop 16

    a = ['abc', 'def', 'ghi', 'jkl']
    if (.not. rank_at(a(3), c_loc(a(3)))) error stop 17
    if (a(3) /= 'Xhi' .or. a(2) /= 'def') error stop 18

    s = 'abcdefg'
    if (.not. fwd_scalar_at(s, c_loc(s))) error stop 19
    if (s /= 'Xbcdefg') error stop 20

    s = 'abcdefg'
    if (.not. fwd_rank_at(s, c_loc(s))) error stop 21
    if (s /= 'Xbcdefg') error stop 22

    a = ['abc', 'def', 'ghi', 'jkl']
    if (.not. fwd_rank_at(a, c_loc(a))) error stop 23
    if (a(1) /= 'Xbc' .or. a(4) /= 'jkl') error stop 24

    print *, "ok"
end program
