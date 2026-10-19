! The ordinary abstract-type counterpart of the child-over-parent and
! concrete-first precedence of traits_runtime_conformance_query_02.
module traits_runtime_conformance_query_02_oracle_m
    implicit none
    type, abstract :: IParent
    contains
        procedure(p_interface), deferred :: p
    end type
    abstract interface
        integer function p_interface(self)
            import :: IParent
            class(IParent), intent(in) :: self
        end function
    end interface
    type, abstract, extends(IParent) :: IChild
    contains
        procedure(c_interface), deferred :: c
    end type
    abstract interface
        integer function c_interface(self)
            import :: IChild
            class(IChild), intent(in) :: self
        end function
    end interface
    type, extends(IChild) :: TChild
    contains
        procedure :: p => child_p
        procedure :: c => child_c
    end type
    type, extends(IParent) :: TParent
    contains
        procedure :: p => parent_p
    end type
    type :: TNone
    end type
contains
    integer function child_p(self)
        class(TChild), intent(in) :: self
        child_p = 1
    end function
    integer function child_c(self)
        class(TChild), intent(in) :: self
        child_c = 2
    end function
    integer function parent_p(self)
        class(TParent), intent(in) :: self
        parent_p = 11
    end function
    integer function parent_first(object) result(r)
        class(*), intent(in) :: object
        select type (x => object)
        class is (IParent)
            r = 100 + x%p()
        class is (IChild)
            r = 200 + x%c()
        class default
            r = 0
        end select
    end function
    integer function child_first(object) result(r)
        class(*), intent(in) :: object
        select type (x => object)
        class is (IChild)
            r = 200 + x%c()
        class is (IParent)
            r = 100 + x%p()
        class default
            r = 0
        end select
    end function
    integer function concrete(object) result(r)
        class(*), intent(in) :: object
        select type (x => object)
        class is (IChild)
            r = 200 + x%c()
        type is (TChild)
            r = 500
        class default
            r = 0
        end select
    end function
end module

program traits_runtime_conformance_query_02_oracle
    use traits_runtime_conformance_query_02_oracle_m
    implicit none
    type(TChild) :: child
    type(TParent) :: parent
    type(TNone) :: none
    if (parent_first(child) /= 202 .or. child_first(child) /= 202) error stop 1
    if (parent_first(parent) /= 111 .or. child_first(parent) /= 111) error stop 2
    if (parent_first(none) /= 0 .or. child_first(none) /= 0) error stop 3
    if (concrete(child) /= 500 .or. concrete(parent) /= 0) error stop 4
    print *, "abstract class guards: child over parent, concrete first"
end program
