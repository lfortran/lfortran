program array_06_cc
    implicit none

    character(4) :: str(3)
    str(1, 2)(:) = '1234'
    str(1,2,3)(:) = '1234'
    print *, "compilation continued despite errors"
end program array_06_cc
