program array_section_05
    integer, allocatable :: A(:,:)
    logical, allocatable :: B(:)
    allocate(A(2,0))
    allocate(B(size(A,2)))
    B = is_prime(A(2,:))
    if (any(shape(B) /= [0])) error stop
    if (all(B) .neqv. .true.) error stop
    if (all(is_prime(A(1:1,:))) .neqv. .true.) error stop
contains 
    elemental logical function is_prime(n)
       integer, intent(in) :: n
       is_prime = .false.
    end function is_prime
end program
