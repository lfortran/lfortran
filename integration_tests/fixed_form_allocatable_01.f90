      program fixed_form_allocatable_01
      integer first, second
      allocatable :: first
      allocatable second
      allocate(first, second)
      first = 11
      second = 13
      if (first + second /= 24) error stop 1
      deallocate(first, second)
      end
