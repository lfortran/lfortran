program spread_scalar_fortran_codegen_implied_do_loops
implicit none
integer(4) :: __1__libasr_index_
integer(4), dimension(1) :: __libasr__created__var__0__array_constructor_
integer(4) :: a
integer(4), dimension(3) :: r
a = 5
__1__libasr_index_ = lbound(__libasr__created__var__0__array_constructor_, 1)
__libasr__created__var__0__array_constructor_(__1__libasr_index_) = a
__1__libasr_index_ = __1__libasr_index_ + 1
r = Spread(__libasr__created__var__0__array_constructor_(1), 1, 3)
print *, r
end program spread_scalar_fortran_codegen_implied_do_loops
