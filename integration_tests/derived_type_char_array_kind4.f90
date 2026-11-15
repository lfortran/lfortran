program f8f
    implicit none
    integer, parameter :: ck = selected_char_kind('ISO_10646')
    type :: w
        character(len=2, kind=ck) :: c(3) = [ck_'ab', ck_'cd', ck_'ef']
    end type
    type(w) :: s
    print *, s%c(1) == ck_'ab', s%c(2) == ck_'cd', s%c(3) == ck_'ef'
end program
