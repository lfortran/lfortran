! A pointer associated with coarrays_54_a's coarray.
module coarrays_54_b
    use coarrays_54_a, only: co_var
    implicit none
    integer, pointer :: p => co_var
end module coarrays_54_b
