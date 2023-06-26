program string_24
type :: string
    character(:), allocatable :: s
end type
type(string) :: z(5)
allocate(character(1) :: z(1)%s)
z(1)%s(1:1) = "x"
print *, z(1)%s(1:1)
if (z(1)%s(1:1) /= "x") error stop
end program

