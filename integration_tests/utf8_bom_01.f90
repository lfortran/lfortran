program utf8_bom_01
integer :: x
x = 42
if (x /= 42) error stop
print *, "UTF-8 BOM test passed"
end program
