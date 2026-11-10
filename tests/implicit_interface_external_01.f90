program prog_ext_sub
integer :: i, y
integer, external :: f
external g
type :: t
    procedure(), pointer, nopass :: p
end type
type(t) :: a
external sub
i = 1
y = f(1)
call g(i)
a%p => sub
call a%p(1)
end program
