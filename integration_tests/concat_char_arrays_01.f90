program concat_char_arrays_01
    implicit none
    character(8) :: a(2), b(3)
    character(16) :: c(2)
    integer :: i
    a = ['aa','a ']
    b = ['bb','b ','  ']

    c = a // b(1:2)

    if (c(1) /= 'aa      bb      ') error stop "c(1) wrong"
    if (c(2) /= 'a       b       ') error stop "c(2) wrong"

end program concat_char_arrays_01