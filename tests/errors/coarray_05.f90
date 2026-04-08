program coarray_05
    type :: t
        integer :: x
    end type t
    type(t) :: s
    ! Should error: x is not a coarray
    s%x = s%x[1]

end program coarray_05