subroutine calc_regression_liner (x, y, num, b, a)
  implicit none 
  real(4) :: &
       & x(num), y(num)

  real (4) ::  xx, xy, avex, avey, undef, a, b, r, stdx, stdy

  integer (4) :: num, in

!  num: sample number
! y: depended variable
! x: independed variable
! b: (partial) regression coefficient
! a: y-intercept
! y=bx+a
! a=ave(y)-b*ave(x)
! b=( sum(xy) - num*ave(x)*ave(y))/(sum(x*x)-num*ave(x)**2)
! r: correlation

!   write(*,*) x(1:10)
!   write(*,*) y(1:10)

   xy=sum(x(:)*y(:),1)
   avex=(sum(x(:),1)/num)
   avey=(sum(y(:),1)/num)
   xx=sum(x(:)*x(:),1)

   b= (xy-(num*avex*avey))/(xx-(num*(avex**2)))
   a=avey-(b*avex)

   stdx=sqrt(sum((x-avex)**2)/(num-1.d0))
   stdy=sqrt(sum((y-avey)**2)/(num-1.d0))
   r=b*stdx/stdy

   write(*,*) '********************'
   write(*,*) 'calculated regression coefficient'
   write(*,*) 'b (regression coeff,)= ',b, 'a (y-intercept)= ',a
   write(*,*) 'r (correlation)= ',r
   write(*,*) '********************'

   return

 end subroutine calc_regression_liner
