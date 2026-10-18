module function_result_preparation_03_m
    implicit none
    integer :: calls = 0
    interface diagonal
        module procedure from_matrix, from_vector
    end interface
contains
    function from_matrix(a) result(r)
        integer, intent(in) :: a(:,:)
        integer :: r(minval(shape(a))), i
        calls = calls + 1
        do i = 1, size(r)
            r(i) = a(i,i)
        end do
    end function

    function from_vector(a) result(r)
        integer, intent(in) :: a(:)
        integer :: r(size(a),size(a)), i
        calls = calls + 1
        r = 0
        do i = 1, size(a)
            r(i,i) = a(i)
        end do
    end function
end module

program function_result_preparation_03
    use function_result_preparation_03_m
    implicit none
    integer :: a(3,4), b(3,3), i
    a = reshape([(i, i=1,12)], shape(a))
    calls = 0
    b = diagonal(diagonal(a))
    if (calls /= 2) error stop 1
    if (any(b(1,:) /= [1,0,0])) error stop 2
    if (any(b(2,:) /= [0,5,0])) error stop 3
    if (any(b(3,:) /= [0,0,9])) error stop 4
    if (any(diagonal(a) /= [1,5,9])) error stop 5
    if (calls /= 3) error stop 6
    if (any(-diagonal(a) /= [-1,-5,-9])) error stop 7
    if (calls /= 4) error stop 8
    if (any(real(diagonal(a)) /= [1.0,5.0,9.0])) error stop 9
    if (calls /= 5) error stop 10
    if (any(abs(diagonal(-a)) /= [1,5,9])) error stop 11
    if (calls /= 6) error stop 12
    if (any(diagonal(a) + diagonal(a) /= [2,10,18])) error stop 13
    if (calls /= 8) error stop 14
    if (any(-cmplx(diagonal(a)) /= cmplx([-1,-5,-9]))) error stop 15
    if (calls /= 9) error stop 16
end program
