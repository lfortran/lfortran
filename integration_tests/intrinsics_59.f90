program test_maxval
implicit none

type :: model_t
    integer, allocatable :: byte_encoder(:)
end type

type(model_t) :: m

allocate(m%byte_encoder(5))
m%byte_encoder = 5
m%byte_encoder(2) = 14

print *, maxval(m%byte_encoder)
if (maxval(m%byte_encoder) /= 14) error stop
end program
