program array_04_cc
implicit none

type :: t
    integer :: a
end type
type(t) :: b
b%a(:) = 1
print *, "compilation continued despite errors"

end program
