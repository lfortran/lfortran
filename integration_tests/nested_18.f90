! Test for nested function global variables passes 
! with function calls in conditions
! Test for both, scalars and arrays
! Test 1: Do-while loop with function call in condition (3 iterations)
! Test 2: IF block with function call in condition

module nested_18_temp
  contains
    subroutine demo()
      implicit none
      integer :: a
      integer :: arr(5)
      integer :: counter

      ! Test 1: Do-While loop - runs 6 times
      ! Check Upodates from Main to Function
      ! And from function to Main
      a = 0
      arr(1:5) = 0
      counter = 0
      GET_ARGS: do while(increment())
        ! Check updates from function to main
        counter = counter + 1
        if (counter == 1 .and. a /= 1 .and. arr(1) /= 1) error stop
        if (counter == 3 .and. a /= 3 .and. arr(3) /= 3) error stop
        if (counter == 5 .and. a /= 5 .and. arr(5) /= 3) error stop
        print *, "Iteration", counter, ": a =", a, ": arr =", arr
        
        ! Update a inside main to check from main to function
        a = a + 1
        arr = arr + 1
        counter = counter + 1
        print *, "Iteration", counter, ": a =", a, ": arr =", arr
        if (counter == 6) exit GET_ARGS
      end do GET_ARGS
      print *, "a =", a
      print *, "arr =", arr
      if (a /= 6) error stop
      if (arr(1) /= 6) error stop

      ! Test 2: IF block with function call in condition
      a = 10
      if (set_value(25)) then
        print *, "In IF body: a =", a
        if (a /= 25) error stop
        a = a + 5
      end if
      print *, a
      if (a /= 30) error stop

      contains
      logical function increment()
        arr = arr + 1
        a = a + 1
        increment = .true.
      end function increment

      logical function set_value(val)
        integer, intent(in) :: val
        a = val
        set_value = .true.
      end function set_value

    end subroutine demo
end module nested_18_temp

program nested_18
  use nested_18_temp
  implicit none
  call demo()
  print *, "Nested variable test passed"
end program nested_18