#define JOIN joined\
_name

#def\
ine SPLIT_DEF 42

program cpp_pre_19
    implicit none
    integer :: joined_name = 7
    integer :: v

    v = JOIN
    if (v /= 7) error stop

    v = SPLIT_DEF
    if (v /= 42) error stop

    print *, JOIN, SPLIT_DEF
end program cpp_pre_19
