program test_string_22
implicit none
type :: string
    character(:), allocatable :: s
end type
type(string) :: a, b
a%s = "abc"
b = a
print *, a%s, b%s
if (a%s /= "abc") error stop
if (b%s /= "abc") error stop
b%s = "xyz"
print *, a%s, b%s
if (a%s /= "abc") error stop
if (b%s /= "xyz") error stop
a%s = "def"
print *, a%s, b%s
if (a%s /= "def") error stop
if (b%s /= "xyz") error stop
end program

