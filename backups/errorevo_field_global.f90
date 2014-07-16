program main
implicit none
real,parameter::lat0=-20.0,interval=5.0
integer,parameter::nx=72,ny=37,nt=17340,inter=45,nyy=9
integer::i,j,t,day
real::lat(nyy),error(nx,nyy)
real::h(nx,ny),hall(nx,ny,inter)
real::eall(inter),e(inter),e1
do j=1,nyy
	lat(j)=lat0+interval*(j-1)
end do

	open(200,file='I:\fb\cfs_true\attr_sd\attr_annual.grd',form='unformatted',access='direct',recl=nx*ny)
		!open(200,file='I:\fb\ncepout\ncepout.grd',form='unformatted',access='direct',recl=nx*ny)
!eall=0.
!	do t=1,nt
		e=0.
		!write(*,*)t
		read(200,rec=1)h
			error=0.
			error(:,:)=h(:,15:23)
			call rmse_space(nx,nyy,error,lat,e1)
			write(*,*)e1
		!eall=eall+e
		!write(*,*)hall(10,10,:)
		!pause
!	end do

!	eall=eall/real(nt)
	close(200)

!	open(100,file='I:\fb\cfs_true\variance\global\rmse_ave_global.grd',form='unformatted',access='direct',recl=inter)
!	write(100,rec=1)e
!	close(100)
!	write(*,*)e
	end



	!!calculate the space_rmse 
subroutine rmse_space(nlon,nlat,x,lat,rmse)
implicit none
real,parameter::pi=3.1415926
integer::nlon,nlat
real::x(nlon,nlat)
real::lat(nlon)
integer::i,j,k
real::total_weight
real::rmse
rmse=0.0
total_weight=0.0
do i=1,nlon
	do j=1,nlat
		rmse=rmse+x(i,j)**2*cos(lat(j)/180.0*pi)
		total_weight=total_weight+cos(lat(j)/180.0*pi)
	end do
end do
rmse=sqrt(rmse/total_weight)
return
end subroutine
