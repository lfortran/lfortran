program coarray_03
! Test: coindex notation on non-coarray variable should produce error
integer :: x
x = x[1]
end program coarray_03
