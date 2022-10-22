program include_02
integer :: included, include(2)
included = 10
include = 10
print *, included, include(1), include(2)
include(1) = 3
print *, included, include(1), include(2)
end program
