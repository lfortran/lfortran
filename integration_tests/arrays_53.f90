program arrays_53
logical :: k(5) = .false.
print *, "k: ", k
if(any(k)) error stop
end program
