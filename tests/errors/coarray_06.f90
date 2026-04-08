program coarray_06
    type :: t
        integer :: x[3,*]
    end type t
    type(t) :: s
    ! Corank = 2, but only 1 coindex provided → ERROR
    s%x[1] = 5
end program coarray_06