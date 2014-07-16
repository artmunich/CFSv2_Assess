!To calculate the predictability limit of coupled lorenz system.
!----------------------------------------------------------------
!--->Variables:
!   nstep: Integrate time step;
!   h: Step size;
!   Tp means predictability time while s & f stand for slow and fast;
!   llyp* mean logarithm of average error read from exsited data;
!------------------------------------------------------------------
!Author: Huai Xiaowei
!Last modified: 22/4/2014
!-------------------------------------------------------------------
program main
implicit none
integer,parameter::nstep=6000
real,parameter::h=0.005
integer::i,nseries(nstep)
real(8)::llyps_c(nstep),llypf_c(nstep),llypa_c(nstep)
real(8)::tps_c,tpf_c,tpa_c
real(8)::llyps_u(nstep),llypf_u(nstep),llypa_u(nstep)
real(8)::tps_u,tpf_u,tpa_u

open(100,file='/disk1/home/ljp/huaixw/coupled_lorenz/mine/error/errorc.dat')
do i=1,nstep
    read(100,9999)nseries(i),llyps_c(i),llypf_c(i),llypa_c(i)
end do
close(100)
open(200,file='/disk1/home/ljp/huaixw/coupled_lorenz/mine/error/erroruc.dat')
do i=1,nstep
    read(200,9999)nseries(i),llyps_u(i),llypf_u(i),llypa_u(i)
end do
9999 format(1x,I10,3f12.4)
close(200)

open(300,file='/disk1/home/ljp/huaixw/coupled_lorenz/mine/tp/tp.txt')
call pre_limit(nstep,llyps_c,tps_c)
write(300,*)'Tps_c=',tps_c*h
call pre_limit(nstep,llypf_c,tpf_c)
write(300,*)'Tpf_c=',tpf_c*h
call pre_limit(nstep,llypa_c,tpa_c)
write(300,*)'Tpa_c=',tpa_c*h
call pre_limit(nstep,llyps_u,tps_u)
write(300,*)'Tps_u=',tps_u*h
call pre_limit(nstep,llypf_u,tpf_u)
write(300,*)'Tpf_u=',tpf_u*h
call pre_limit(nstep,llypa_u,tpa_u)
write(300,*)'Tpa_u=',tpa_u*h
close(300)
end program

!-------Subroutine process: determining the limit of predictability from
!-------the error increasing series.
subroutine pre_limit(nstep,llyp,tlength)
implicit none
integer::nstep,i
real(8)::llyp(nstep),tlength
real(8)::saturation,ratio
ratio=0.95
saturation=0.0

do i=nstep-299,nstep
    saturation = saturation + llyp(i)  
end do
saturation = saturation/float(300)*ratio

do i=1,nstep
    if(llyp(i).le.saturation.and.llyp(i+1).ge.saturation)then
        goto 111
    end if
end do
111   tlength=((llyp(i+1)-saturation)*i+(saturation-llyp(i))*(i+1))&
    	 /(llyp(i+1)-llyp(i))
return
end subroutine

