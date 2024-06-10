program intrinsics_247
implicit none
    
class(*), allocatable :: a, b
allocate(a, source=10)
allocate(b, source=3.14)
print*, same_type_as(a, b)
if (same_type_as(a, b) .neqv. .false.) error stop

deallocate(a, b)
allocate(a, source=10)
allocate(b, source=10)
print*, same_type_as(a, b)
if (same_type_as(a, b) .neqv. .true.) error stop
end program