program associate_43
  implicit none
  integer :: b

  ! Test: array constructor with nested ACs containing implied-do
  ! whose bounds are runtime (associate) variables
  associate(n => 2)
    associate(arr => [ [(1, b=1,n)], [(2, b=1,n)] ])
      if (size(arr) /= 4) error stop
      if (arr(1) /= 1) error stop
      if (arr(2) /= 1) error stop
      if (arr(3) /= 2) error stop
      if (arr(4) /= 2) error stop
    end associate
  end associate
end program
