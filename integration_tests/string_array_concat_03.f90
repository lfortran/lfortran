program string_array_concat_03
    ! Concatenation with an array-valued character function whose result
    ! length and shape depend on its dummy arguments must keep the lengths
    ! of all operands (issue #14715).
    implicit none
    character(len=30) :: line
    character(len=1) :: arr(2)

    call check_suffix(f(["a", "b"]) // "'")
    call check_both("'" // f(["a", "b"]) // "'")

    arr = ["c", "d"]
    call check_suffix_cd(f(arr) // "'")

    write(line, "(*(1x,a))") "'" // munge(["a", "b"], ["1", "2"], "-") // "'"
    if (line /= " 'a-1' 'b-1' 'a-2' 'b-2'") error stop 1

contains

    subroutine check_suffix(c)
        character(len=*), intent(in) :: c(:)
        if (len(c) /= 2) error stop 2
        if (size(c) /= 2) error stop 3
        if (c(1) /= "a'" .or. c(2) /= "b'") error stop 4
    end subroutine

    subroutine check_both(c)
        character(len=*), intent(in) :: c(:)
        if (len(c) /= 3) error stop 5
        if (size(c) /= 2) error stop 6
        if (c(1) /= "'a'" .or. c(2) /= "'b'") error stop 7
    end subroutine

    subroutine check_suffix_cd(c)
        character(len=*), intent(in) :: c(:)
        if (len(c) /= 2) error stop 8
        if (c(1) /= "c'" .or. c(2) /= "d'") error stop 9
    end subroutine

    function f(x) result(y)
        character(len=*), intent(in) :: x(:)
        character(len=len(x)) :: y(size(x))
        y = x
    end function

    function munge(xx, yy, delim) result(xy)
        character(len=*), intent(in) :: xx(:), yy(:), delim
        character(len=len(xx)+len(yy)+len(delim)) :: xy(size(xx)*size(yy))
        integer :: ix, iy, nx
        nx = size(xx)
        forall (ix=1:size(xx), iy=1:size(yy)) &
            xy(ix+(iy-1)*nx) = trim(xx(ix)) // delim // trim(yy(iy))
    end function

end program string_array_concat_03
