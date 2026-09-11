      MODULE G
        TYPE GT ! trailing comment must not defeat opener detection
          INTEGER N
        END TYPE GT
        TYPE(GT), SAVE:: DAT(10)
      END MODULE G
