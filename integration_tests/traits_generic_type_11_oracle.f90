module traits_generic_type_11_refs
    implicit none
    type :: First
        integer :: value = 0
    end type
    type :: Second
        real :: value = 0.0
    end type
    type :: Ref_First
        type(First), pointer :: scalar => null()
        type(First), pointer :: vector(:) => null()
        type(First), pointer :: matrix(:,:) => null()
    end type
    type :: Ref_Second
        type(Second), pointer :: scalar => null()
        type(Second), pointer :: vector(:) => null()
        type(Second), pointer :: matrix(:,:) => null()
    end type
    type :: Ref_integer
        integer, pointer :: scalar => null()
        integer, pointer :: vector(:) => null()
        integer, pointer :: matrix(:,:) => null()
    end type
    type :: Ref_real
        real, pointer :: scalar => null()
        real, pointer :: vector(:) => null()
        real, pointer :: matrix(:,:) => null()
    end type
end module

program traits_generic_type_11
    use traits_generic_type_11_refs
    implicit none
    type(Ref_First) :: a, again
    type(Ref_Second) :: b
    type(Ref_integer) :: integers
    type(Ref_real) :: reals
    type(First), target :: first_scalar, first_vector(3), first_matrix(2,3)
    type(Second), target :: second_scalar, second_vector(2), second_matrix(3,2)
    integer, target :: int_scalar = 4, int_vector(2) = [5,6], int_matrix(2,2)
    real, target :: real_scalar = 1.5, real_vector(2) = [2.5,3.5]

    if (associated(a%scalar) .or. associated(a%vector) .or. associated(a%matrix)) error stop 1
    if (associated(b%scalar) .or. associated(b%vector) .or. associated(b%matrix)) error stop 2
    if (associated(integers%scalar) .or. associated(integers%vector) .or. &
        associated(integers%matrix)) error stop 3
    if (associated(reals%scalar) .or. associated(reals%vector) .or. associated(reals%matrix)) error stop 4
    if (associated(again%scalar) .or. associated(again%vector) .or. associated(again%matrix)) error stop 5

    a%scalar => first_scalar
    a%vector => first_vector
    a%matrix => first_matrix
    b%scalar => second_scalar
    b%vector => second_vector
    b%matrix => second_matrix
    integers%scalar => int_scalar
    integers%vector => int_vector
    integers%matrix => int_matrix
    reals%scalar => real_scalar
    reals%vector => real_vector
    again%scalar => first_scalar

    a%scalar%value = 7
    a%vector(3)%value = 8
    a%matrix(2,3)%value = 9
    b%scalar%value = 1.25
    b%vector(2)%value = 2.25
    b%matrix(3,2)%value = 3.25
    integers%scalar = 10
    integers%vector(2) = 11
    integers%matrix(2,1) = 12
    reals%scalar = 4.5
    reals%vector(2) = 5.5
    if (first_scalar%value /= 7 .or. again%scalar%value /= 7) error stop 6
    if (first_vector(3)%value /= 8 .or. first_matrix(2,3)%value /= 9) error stop 7
    if (second_scalar%value /= 1.25 .or. second_vector(2)%value /= 2.25 .or. &
        second_matrix(3,2)%value /= 3.25) error stop 8
    if (int_scalar /= 10 .or. int_vector(2) /= 11 .or. int_matrix(2,1) /= 12) error stop 9
    if (real_scalar /= 4.5 .or. real_vector(2) /= 5.5) error stop 10
    if (.not. associated(a%matrix, first_matrix)) error stop 11
    if (.not. associated(b%matrix, second_matrix)) error stop 12
    if (any(shape(a%matrix) /= [2,3]) .or. any(shape(b%matrix) /= [3,2])) error stop 13
    if (associated(again%vector) .or. associated(again%matrix)) error stop 14
    nullify(a%scalar)
    if (associated(a%scalar) .or. .not. associated(again%scalar, first_scalar)) error stop 15
    print '(a)', 'generic null initializers: OK'
end program
