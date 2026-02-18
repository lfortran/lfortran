program infer_walrus_error_02
    implicit none
    type :: t
        integer :: n
    end type
    type(t) :: obj
    a := obj
end program
