! The optional construct name on an end statement of a TEMPLATE or a
! REQUIREMENT construct must match the name on the opening statement
! (J3/26-007r1, 16.2 and 16.6):
!
!     R1603 end-template-stmt     is  END TEMPLATE [ template-name ]
!     C1602 If a template-name appears in an end-template-stmt, it shall be
!           the same as that in the corresponding template-stmt.
!
!     R1635 end-requirement-stmt  is  END REQUIREMENT [ requirement-name ]
!     C1638 If a requirement-name appears in the end-requirement-stmt, it
!           shall be the same as that in the corresponding requirement-stmt.
!
! See integration_tests/template_end_name_01.f90 for the accepted spellings.

module template_end_name_1

    requirement r {t}
        deferred type :: t
    end requirement not_r  ! {Error} End requirement name does not match requirement name

    template tmpl(u)
        deferred type :: u
    end template not_tmpl  ! {Error} End template name does not match template name

end module
