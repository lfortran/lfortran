module m_class_declarations

    type :: my_type
    end type

contains

    subroutine some_subroutine(self)

        ! _Polymorphic_ variable
        class(my_type) :: self

        ! Local variable
        type(my_type) :: local_var

        ! allocatable
        class(my_type), allocatable :: allocatable_var

        ! pointer
        class(my_type), pointer :: pointer_var

    end subroutine


end module m_class_declarations
