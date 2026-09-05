module pdt_18_m
    implicit none

    type inner_t(k)
        integer, kind :: k
        real(k) :: value
    end type

    type outer_t(k)
        integer, kind :: k
        type(inner_t(k)) :: member = inner_t(k)(real(1, k))
    end type
end module

program pdt_18
    use pdt_18_m
    implicit none
    type(outer_t(4)) :: value4
    type(outer_t(8)) :: value8

    if (value4%member%k /= 4 .or. kind(value4%member%value) /= 4) error stop
    if (value8%member%k /= 8 .or. kind(value8%member%value) /= 8) error stop
    if (value4%member%value /= 1.0) error stop
    if (value8%member%value /= 1.0_8) error stop
end program
