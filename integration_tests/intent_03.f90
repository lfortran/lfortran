module prime_numbers_intent_03_module
   integer, parameter :: p40001_to_50000(10) = &
        [ 40009, 40011, 40013, 40027, 40029, 40039, 40051, 40057, 40063, 40069 ]

    contains

    ! Return the n-th prime number
    function prime() result(prime_number)
        integer :: prime_number
        prime_number = next_prime(p40001_to_50000(4))
    end function prime

    elemental integer function next_prime(n)
       integer, intent(in) :: n
       next_prime = n + 9
    end function next_prime

end module prime_numbers_intent_03_module

program intent_03

  use prime_numbers_intent_03_module
  print *, prime()
  if ( prime() /= 40036 ) error stop
end program

