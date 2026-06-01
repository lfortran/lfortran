program arrays_reshape_43
  implicit none
  integer, allocatable :: rank1(:)
  integer, pointer :: rank2(:, :)

  allocate(rank1(5), source=0)
  rank1 = [1, 2, 3, 4, 5]
  associate(rank2 => reshape(rank1, [shape(rank1), 1]))
    if (size(rank2, 1) /= 5 .or. size(rank2, 2) /= 1) error stop
    if (rank2(1, 1) /= 1 .or. rank2(2, 1) /= 2 .or. rank2(3, 1) /= 3 .or. rank2(4, 1) /= 4 .or. rank2(5, 1) /= 5) error stop  
  end associate
end program arrays_reshape_43
