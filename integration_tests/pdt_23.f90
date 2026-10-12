! Character components whose length depends on a kind type parameter
module pdt_23_mod
implicit none

type :: label(k)
    integer, kind :: k = 3
    character(len=k) :: name
    character(len=2*k+1) :: wide
    character(len=k) :: names(2)
end type

type :: holder(n)
    integer, kind :: n = 4
    type(label(n)) :: inner
end type

end module

program pdt_23
use pdt_23_mod, only: label, holder
implicit none

type :: tag(k)
    integer, kind :: k
    character*(k) :: s
end type

type(label) :: a
type(label(5)) :: b
type(holder) :: h
type(tag(2)) :: t

a%name = "abcdef"
a%wide = "abcdefghijklmnop"
b%wide = "abcdefghijklmnop"
b%name = "abcdef"
a%names = ["xyzw", "uvwx"]
b%names(1) = "pq"
h%inner%name = "123456"
t%s = "hello"

if (len(a%name) /= 3 .or. a%name /= "abc") error stop 1
if (len(b%name) /= 5 .or. b%name /= "abcde") error stop 2
if (len(a%wide) /= 7 .or. a%wide /= "abcdefg") error stop 3
if (len(b%wide) /= 11 .or. b%wide /= "abcdefghijk") error stop 4
if (len(a%names) /= 3 .or. a%names(2) /= "uvw") error stop 5
if (len(b%names) /= 5 .or. b%names(1) /= "pq   ") error stop 6
if (len(h%inner%name) /= 4 .or. h%inner%name /= "1234") error stop 7
if (len(t%s) /= 2 .or. t%s /= "he") error stop 8
print *, a%name, " ", b%name, " ", a%wide, " ", b%wide, " ", h%inner%name, " ", t%s
end program
