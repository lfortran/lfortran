! Uses only the contracts: the concrete types of the provider and their
! conformances are found through their runtime conformance tables.
module traits_runtime_conformance_query_separate_01_consumer
    use traits_runtime_conformance_query_separate_01_contracts
    implicit none
contains
    integer function classify_any(object) result(r)
        class(*), intent(in) :: object
        select type (x => object)
        class is (IValue)
            r = 100 + x%value()
        class is (IScaled)
            r = 200 + x%scaled(2)
            select type (y => x)
            class is (ILabel)
                r = r + 1000 * y%label()
            end select
        class is (ILabel)
            r = 300 + x%label()
        class default
            r = -1
        end select
    end function
    integer function classify_base(object) result(r)
        class(Base), intent(inout) :: object
        select type (x => object)
        class is (IScaled)
            call x%grow(5)
            r = x%value()
        class is (IValue)
            r = x%value()
        class default
            r = -1
        end select
    end function
    integer function classify_value(view) result(r)
        class(IValue), intent(in) :: view
        select type (x => view)
        class is (ILabel)
            r = x%label() + view%value()
        class default
            r = -1
        end select
    end function
end module
