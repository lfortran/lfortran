program character_39
    ! array constructor with shorter strings assigned to character(8) array (#3862)
    implicit none
    character(8) :: a(2)
    a = ['aa', 'a ']
    print *, len(a(1))
    if (len(a(1)) /= 8) error stop
    if (len(a) /= 8) error stop
    if (a(1) /= 'aa') error stop
    if (a(2) /= 'a') error stop
    if (a(1) /= 'aa      ') error stop
    if (a(1)(3:8) /= '      ') error stop
    if (a(2)(2:8) /= '       ') error stop
    if (len_trim(a(1)) /= 2) error stop
    if (len_trim(a(2)) /= 1) error stop
end program
