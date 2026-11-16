program iostat_array_component_01
    implicit none
    type :: t
        integer :: codes(2)
        character(len=64) :: msgs(2)
    end type t
    type(t) :: x
    integer :: u, n

    x%codes = -1
    x%msgs = ""
    write(*, *, iostat=x%codes(2)) 1
    if (x%codes(1) /= -1) error stop 1
    if (x%codes(2) /= 0) error stop 2

    x%codes = -1
    open(newunit=u, file="iostat_array_component_01.tmp", status="replace", &
        iostat=x%codes(1), iomsg=x%msgs(2))
    if (x%codes(1) /= 0) error stop 3
    if (x%codes(2) /= -1) error stop 4
    if (len_trim(x%msgs(2)) /= 0) error stop 5

    rewind(u)
    read(u, *, iostat=x%codes(2), iomsg=x%msgs(1)) n
    if (x%codes(2) == 0) error stop 6
    if (len_trim(x%msgs(1)) == 0) error stop 7
    if (len_trim(x%msgs(2)) /= 0) error stop 8
    close(u, status="delete")

    print "(a)", "ok"
end program iostat_array_component_01
