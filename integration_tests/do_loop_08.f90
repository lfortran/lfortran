program do_loop_08
    implicit none

    type :: string_t
        character(len=:), allocatable :: string_
    end type

    type(string_t) :: body(1,2)
    type(string_t), allocatable :: tbl(:)

    body(1,1)%string_ = "a"
    body(1,2)%string_ = "b"

    tbl = make_table(body)

    if (size(tbl) /= 3) error stop
    if (tbl(1)%string_ /= "x") error stop
    if (tbl(2)%string_ /= "x") error stop
    if (tbl(3)%string_ /= "x") error stop

    print *, "ok"

contains

    function make_table(body_cells) result(lines)
        type(string_t), intent(in) :: body_cells(:,:)
        type(string_t) :: lines(size(body_cells,1) + rank(body_cells))
        integer :: row

        do row = 1, size(lines)
            lines(row)%string_ = "x"
        end do
    end function

end program
