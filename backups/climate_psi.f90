program main
implicit none
integer,parameter::mx=144,my=73,ntt=91980,nyear=63,nday=1460
real::psi(ntt)
integer::i,j,t,iy,iday,flag,irec
logical::s
real::climate(mx,my),climate1

!----input data
open(unit=20,file='I:\fb\ncepout\ncepout_all_series_1948_2010.grd',&
form='unformatted',access='direct',recl=ntt)

!----output data
open(unit=30,file='I:\barotropic\QG\climate/hgt500_ob_cli.dat',&
form='unformatted',access='direct',recl=mx*my)


!-----anamoly
do j=1,my
        do i=1,mx
        write(*,*)i,j
        irec=(j-1)*mx+i
        read(20,rec=irec)psi(:)

        flag=0
        climate1=0.
        do iy=1,nyear
                do t=1,nday

                iday=(iy-1)*nday+t
                call season(t,s)
                if(s) then
                        flag=flag+1
                        climate1=climate1+psi(iday)
                endif

                enddo
        end do

        climate(i,j)=climate1/real(flag)
        end do
end do
write(30,rec=1)climate


end program

!=====================================================
!calculate the climate cycle. The raw series is x(m).
!input:    
!---m:the number of the time series
!---x(m):the series
!---nt:the cycle
!output:      
!---y(nt)     
subroutine clim_cycle(m,nt,x,y)
implicit none
integer::m,nt,i,j
integer::flag(nt)
real::x(m)    
real::y(nt)
real::summ
summ=0.0
flag=0
y=0.
do i=1,m
        j=mod(i,nt)
        if(j==0)then
        j=nt
        flag(j)=flag(j)+1
        y(j)=y(j)+x(i)
        else
        flag(j)=flag(j)+1
        y(j)=y(j)+x(i)
        endif
end do
do j=1,nt
        y(j)=y(j)/real(flag(j))
end do
return
end subroutine

!calculate the anomaly
!input:
!---m:the number of the time series
!---x(m):the series
!---nt:the cycle
!---y(nt)
!output:
!---out(m)anomaly series for removing the annual cycle
subroutine anomaly(m,nt,x,y,xout)
implicit none
integer::m,nt,i,j,flag
real::x(m),y(nt)
real::xout(m)
real::summ
summ=0.0
flag=0
do i=1,m
        j=mod(i,nt)   
        if(j==0)then
        j=nt
        xout(i)=x(i)-y(j)
        else
       xout(i)=x(i)-y(j)
        end if
end do
return
end subroutine

subroutine season(i,s)
implicit none
integer::i
logical::s
s=.false.
!if(i>=605.and.i<=972)then       !summer
if(i>=1.and.i<=59)then          !winter
!if(i>=237.and.i<=604)then         !spring
!if(i>=973.and.i<=1336)then
s=.true.
else if(i>=335.and.i<=365) then    !winter
s=.true.
endif
return
end subroutine


!calculate the average,variance,standard deviation!
subroutine meanvar(m,x,mx)
implicit none
integer::m,i
real::x(m)
real::ax,vx,mx
real::summ
summ=0.0
do i=1,m
        summ=summ+x(i)
end do
ax=summ/real(m)
summ=0.0
do i=1,m
        summ=(x(i)-ax)**2+summ
end do
vx=summ/real(m)
mx=sqrt(vx)
return
end subroutine
