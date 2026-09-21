! Declaring the same deferred type name twice inside a requirement.
!
! The deferred-type path of SymbolTableVisitor::visit_DerivedType added the
! symbol unconditionally, and SymbolTable::add_symbol asserts the name is still
! free, so this invalid input reached an internal assertion instead of a
! diagnostic. In a Release build, where the assertion is compiled out, the
! duplicate silently overwrote the first declaration.
!
! A template declaring a duplicate deferred type goes through the very same
! branch of the same function; see integration_tests/template_07.f90 for the
! accepted spellings.

module deferred_type_duplicate_1
    implicit none

    requirement r {t}
        deferred type :: t
        deferred type :: t  ! {Error} Symbol is already declared in the same scope
    end requirement

end module
