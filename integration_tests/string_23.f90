program string_23
type :: string
    character(:), allocatable :: s
end type
type(string) :: z
allocate(character(1) :: z%s)
z%s(1:1) = "x"
print *, z%s(1:1)
if (z%s(1:1) /= "x") error stop
end program

