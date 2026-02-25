#define CONCAT2(x,y) x##y
#define CONCAT3(x,y,z) x##y##z
program cpp_pre_08
    implicit none
    integer :: foo_bar, abc_def_ghi
    foo_bar = 42
    abc_def_ghi = 99
    if (CONCAT2(foo,_bar) /= 42) error stop
    if (CONCAT3(abc,_def,_ghi) /= 99) error stop
    print *, "ok"
end program
