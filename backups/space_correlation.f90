program main
implicit none
integer,parameter::mm=502,nn=330,tt=6,cmm=499,cnn=331
real,parameter::lat0=15.0,interval=0.15
real::h500_obser(mm,nn,tt),h500_model(mm,nn,tt),x(cmm,nn),y(cmm,nn)
real::climate(cmm,cnn)
real::lat(nn),r6(tt)
real::r
integer::i,j,t
do j=1,nn
	lat(j)=lat0+interval*(j-1)
end do
open(1,file="/dgpfs/fs2/wangjch/GRAPES_MESO_OPER/fengjie/uvth_500_ob.grd",&
access='direct',form='unformatted',recl=mm*nn*4)
open(2,file="/dgpfs/fs2/wangjch/GRAPES_MESO_OPER/fengjie/uvth_500_120.grd",&
access='direct',form='unformatted',recl=mm*nn*4)
open(3,file="/dgpfs/fs2/wangjch/GRAPES_MESO_OPER/fengjie/interpolate/uvth_500_climate_b.grd",&
access='direct',form='unformatted',recl=cmm*cnn*4)
do t=1,tt
	read(1,rec=(t-1)*4+4)h500_obser(:,:,t)
end do
close(1)
write(*,*)h500_obser(1:100,1,1)
do t=1,tt
	read(2,rec=(t-1)*4+4)h500_model(:,:,t)
end do
close(2)
read(3,rec=4)climate(:,:)
close(3)
do t=1,tt
	do j=1,nn
		do i=1,cmm
		x(i,j)=h500_model(i,j,t)-climate(i,j)
		y(i,j)=h500_obser(i,j,t)-climate(i,j)
		!call correlation_space(cmm,nn,x,y,lat,r)
		r6(t)=r
		end do
	end do
end do
!open(10,file="/dgpfs/fs2/wangjch/GRAPES_MESO_OPER/fengjie/h500_acc_120.grd",&
!access='direct',form='unformatted',recl=tt*4)
!write(10,rec=1)r6(:)
!close(10)
end





!-----*----------------------------------------------------6---------7--
!     For the correlation coefficient r between two series 
!       x(i) and y(i), where i=1,...,n.
!     input: n,x(n),y(n)
!       n: number of time series
!       x(n): raw series 
!       y(n): raw series 
!     output: r
!       r: space correlation coefficient between x and y
!     By Dr. Wang Jincheng, July 28, 2010.
subroutine correlation_space(nlon,nlat,x,y,lat,r)
   implicit none
   real,parameter::pi=3.1415926
   integer::nlon,nlat
   real   ::x(nlon,nlat),y(nlon,nlat)
   real   ::lat(nlat)
   integer::i,j,k
   real   ::sxy
   real   ::ax,sx,vx
   real   ::ay,sy,vy
   real   ::r
   
   call meanvar_space(nlon,nlat,lat,x,ax,sx,vx)
   call meanvar_space(nlon,nlat,lat,y,ay,sy,vy)
   sxy=0.0
   do i=1,nlon
      do j=1,nlat
         sxy=sxy+(x(i,j)-ax)*(y(i,j)-ay)*cos(lat(j)/180.0*pi)
     enddo
   enddo

   r=sxy/(sx*sy)
   return

end

!-----*----------------------------------------------------6---------7--
!     Computing the mean ax, standard deviation sx
!       and variance vx of a series x(i) (i=1,...,n).
!     input: n and x(n)
!       n: number of raw series
!       x(n): raw series
!     output: ax, sx and vx
!       ax: the mean value of x(n)
!       sx: the standard deviation of x(n)
!       vx: the variance of x(n)
!     By Dr. LI Jianping, May 6, 1998.
subroutine meanvar_space(nlon,nlat,lat,x,ax,sx,vx)
   implicit none
   real,parameter::pi=3.1415926
   integer::nlon,nlat
   real   ::lat(nlat)
   real   ::x(nlon,nlat)
   real   ::ax,sx,vx
   integer::i,j,k
   ax=0.

   do i=1,nlon
      do j=1,nlat
         ax=ax+x(i,j)  
      enddo
   enddo
   ax=ax/float(nlon*nlat)
   vx=0.
   sx=0.	  
   do i=1,nlon
      do j=1,nlat
         vx=vx+(x(i,j)-ax)**2*cos(lat(j)/180.0*pi)
     enddo
   enddo
   sx=sqrt(vx)
   return
end

