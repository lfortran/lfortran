program document_symbols2
    implicit none
    type :: aa
        integer :: x
        integer :: y
    end type aa
    type(aa) :: type_inst
    type_inst%x = 3
    type_inst%y = 4
end program document_symbols2
