SUBROUTINE INT(a,b,c)
!----------------------------------------------------------------!
! by Ethan Nelson
! WALRUS Project
!
! SUBROUTINE INT
! Calculates intersection from top of the profile down.
!
! Inputs:
!   a - profile for intersection to be computed
!   b - value of x for intersection
!
! Common Variables:
!   nht - number of points in profile
!   heights - array of height values for each level
!
! Outputs:
!   c - value of y at intersection
!
! See http://github.com/ethan-nelson/walrusutils for more info.
!----------------------------------------------------------------!
IMPLICIT NONE
INTEGER :: i, index, index2
REAL(8), INTENT(IN) :: a(nph), b
REAL(8), INTENT(OUT) :: c
REAL(8) :: d(2), e(2), revh(nph), reva(nph)

revh = heights(nph:1:-1)
reva = a(nph:1:-1)

i = 1
index = -1
index2 = -1

DO WHILE ((i < nht) .AND. (reva(i) < b))
   index = i
   i = i+1
END DO

IF ((i < nht) .AND. (index == 0)) THEN
   DO WHILE ((i < nht) .AND. (reva(i) >= b))
      index2 = i
      i = i+1
   END DO
END IF

IF (index >= 1) THEN ! If profile was initially < b
   d = (/reva(index), revh(index)/)
   e = (/reva(index+1), revh(index+1)/)
   CALL INTCALC(a,b,intersect,e)
ELSE IF (index2 >= 1) THEN ! If profile was initially > b
   d = (/reva(index2), revh(index2)/)
   e = (/reva(index2+1), rev(index2+1)/)
   CALL INTCALC(d,e,b,c)
 ELSE
   c = -999
   PRINT *,'ERROR DETECTED IN INTERSECT.'
END

END SUBROUTINE INT
