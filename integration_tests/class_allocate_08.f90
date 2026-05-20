program class_allocate_08
    use iso_fortran_env, only: real32
    implicit none
 
    type :: my_type
       integer :: id
       real(real32) :: val
    end type my_type
 
    class(*), allocatable :: data_poly(:,:)
    integer :: i, j
 
    data_poly = get_sample(2, 3)
 
    if (.not. allocated(data_poly)) error stop "data_poly not allocated"
    if (size(data_poly, 1) /= 2) error stop "wrong dim 1"
    if (size(data_poly, 2) /= 3) error stop "wrong dim 2"
 
    select type (data_poly)
    type is (my_type)
       do i = 1, 2
          do j = 1, 3
             if (data_poly(i, j)%id /= i*10 + j) error stop "wrong id"
             if (abs(data_poly(i, j)%val - real(i + j, real32)) &
                   > 1.0e-6_real32) error stop "wrong val"
          end do
       end do
    class default
       error stop "wrong dynamic type"
    end select
 
    print *, "PASS"
 
 contains
 
    function get_sample(m, n) result(sample)
       integer, intent(in) :: m, n
       type(my_type), dimension(:,:), allocatable :: sample
       integer :: ii, jj
       allocate(sample(m, n))
       do ii = 1, m
          do jj = 1, n
             sample(ii, jj)%id = ii*10 + jj
             sample(ii, jj)%val = real(ii + jj, real32)
          end do
       end do
    end function get_sample
 
 end program class_allocate_08