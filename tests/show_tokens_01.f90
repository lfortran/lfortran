program show_tokens_01
implicit none
logical :: l
byte :: b
doublecomplex :: z
b = 1
z = (1, 2)
l = .true. .xor. .false.
call s(b)
contains
subroutine s(q)
byte, intent(in out) :: q
q = q + 1
end subroutine
end program
