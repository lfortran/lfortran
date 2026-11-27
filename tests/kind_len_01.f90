program kind_len_01
    implicit none
    character(len=8) :: s
    integer(len('1234')) :: a
    integer(len(s)) :: b
    if (kind(a) /= 4) error stop 1
    if (kind(b) /= 8) error stop 2
end program kind_len_01
