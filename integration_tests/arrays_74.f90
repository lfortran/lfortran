! Test using type member in array constructor 
program arrays_74
    type :: m
        integer :: aa(3)
    end type m

    integer :: j(5)
    type(m) :: tt

    tt%aa = [3, 4, 5]
    j = [ 1,2, tt%aa]
end program arrays_74