SUBROUTINE INT(a,b,c,d)
!----------------------------------------------------------------!
! by Ethan Nelson
! WALRUS Project
!
! SUBROUTINE INT
! Calculates y at x on a line defined by two points.
!
! Inputs:
!   a - first point [x,y].
!   b - secont point [x,y].
!   c - x value to find y on line.
!
! Outputs:
!   d - y value at c.
!
! See http://github.com/ethan-nelson/walrusutils for more info.
!----------------------------------------------------------------!
IMPLICIT NONE
REAL(8), INTENT(IN)  :: a(2), b(2), c
REAL(8), INTENT(OUT) :: d

!Solving two-point linear equation intersection
out = ((b(2)-a(2))*c+(b(1)*a(2)-a(1)*b(2)))/(b(1)-a(1))

END SUBROUTINE INT
