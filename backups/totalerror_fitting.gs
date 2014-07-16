
'reinit'

'enable print I:\fb\cfs_true\variance\fitting_diffalfa.gmf'
infile1='I:\fb\cfs_true\variance\global\fitting_global.ctl'
infile2='I:\fb\cfs_true\variance\NH\fitting_NH.ctl'
infile3='I:\fb\cfs_true\variance\SH\fitting_SH.ctl'
infile4='I:\fb\cfs_true\variance\TRO\fitting_TRO.ctl'
infile5='I:\fb\cfs_true\variance\global\rmse_ave_global.ctl'
infile6='I:\fb\cfs_true\variance\NH\rmse_ave_NH.ctl'
infile7='I:\fb\cfs_true\variance\SH\rmse_ave_SH.ctl'
infile8='I:\fb\cfs_true\variance\TRO\rmse_ave_TRO.ctl'



*--------- Parameters ---------------------------------------------
r1=0.7
lx=4.4
ly=lx*r1
x01=1
x02=x01+lx
y02=8
y01=y02-ly
dx=lx+0.5
dy=ly+0.5

*------------------------------------------------------------------

* Figure 1-----
v1=0
v2=140
yl=20
cli=108.481
commonxy(x01,x02,y01,y02,1,1,dx,dy)
uwnd(infile1,infile5,v1,v2,yl,cli)

* Figure 2-----
v1=0
v2=140
yl=20
cli=121.2735
commonxy(x01,x02,y01,y02,2,1,dx,dy)
uwndI(infile2,infile6,v1,v2,yl,cli)

* Figure 3-----
v1=0
v2=160
yl=20
cli=136.626
commonxy(x01,x02,y01,y02,1,2,dx,dy)
uwndI(infile3,infile7,v1,v2,yl,cli)


* Figure 4-----
v1=0
v2=40
yl=5
cli=21.66417
commonxy(x01,x02,y01,y02,2,2,dx,dy)
uwndI(infile4,infile8,v1,v2,yl,cli)

*'cbarn 1 1 7.2 5.5'




*'set line 2 0 10'
*'cbar_line -x 7.0 -y 3.0 -c 1 1 1 2 3 -l 1 0 4 2 2 -m  0 2 0 0 0 -t  *"model fc" "fit line" "LDA" "GAR" "SD" -p'


*'set line 2 0 10'
*'cbar_line -x 7.0 -y 3.0 -c 2 3 7 8 4 5 1 -l 1 1 1 1 1 1 1 -m 0 0 0 0 0 0 0 -t  "1D3" "5D3" "1D4" "5D4" "1D5" "5D5" "1D6" -p'
*'set line 2 0 10'
*'cbar_line -x 7.0 -y 3.0 -c 2 3 4 -l 1 1 1 -m 0 0 0 -t  "Random" "BV" "NLLV" -p'


*'set line 2 0 8'
*'cbar_line -x 8.0 -y 7.0 -c 1 2 3 4 8 -l 1 1 1 1 1 -m 0 0 0 0 0 -t *"Control" "bv_1" "bv_2" "bv_3" "bv_4" -p'


'set string 1 l 5'
'set strsiz 0.14 0.16'
'draw string 1.3 5.3 (a) Global'

'set string 1 l 5'
'set strsiz 0.14 0.16'
'draw string 6.2 5.3 (b) NH'

'set string 1 l 5'
'set strsiz 0.14 0.16'
'draw string 1.3 1.6 (c) SH'

'set string 1 l 5'
'set strsiz 0.14 0.16'
'draw string 6.2 1.6 (d) TRO'

'set string 1 l 5'
'set strsiz 0.18 0.20'
'draw string 3.8 0.8 Forecast Lead Time ( Day )'

'set string 1 l 5 90'
'set strsiz 0.18 0.20'
'draw string 0.2 3.4 RMS error ( m )'


'print'
'disable print'

'print I:\fb\cfs_true\variance\fitting_diffalfa.eps'

*=================Framework=========================================
function commonxy(x01,x02,y01,y02,nx,ny,dx,dy)
xmin=x01+(nx-1)*dx
xmax=x02+(nx-1)*dx
ymin=y01-(ny-1)*dy
ymax=y02-(ny-1)*dy
'set parea 'xmin' 'xmax' 'ymin' 'ymax' '
'set grads off'
'set frame on'
'set grid off'
'set xlab on'
'set ylab on'

'set annot  1  4'
'set xlopts 1  5  0.15'
'set ylopts 1  5  0.15' 
 

return
*===============================================================
function uwnd(infile1,infile2,v1,v2,yl,cli)
'open 'infile2''
'set grid on'
'set xaxis 0 44 5'
'set vrange 'v1' 'v2''
'set ylint 'yl''

'set t 1 45'
'set ccolor 1'
'set cthick 10'
'set cstyle 1'
'set cmark 0'
'd hgt'

'set ccolor 2'
'set cthick 10'
'set cstyle 2'
'set cmark 0'
'define dummy='cli''
'd dummy'

'set ccolor 3'
'set cthick 10'
'set cstyle 2'
'set cmark 0'
'define dummy='cli*0.7071''
'd dummy'

'close 1'

'open 'infile1''
'set ccolor 1'
'set cthick 10'
'set cstyle 0'
'set cmark 2'
'set t 1 45'
'd hgt'

'close 1'


'set line 2 0 10'
'cbar_line -x 3.7 -y 5.8 -c 1 1 2 3 -l 1 0 2 2 -m  0 2 0 0 -t  "Model Fc" "Fit Line" "GAR" "SD"'

return

*===============================================================
function uwndI(infile1,infile2,v1,v2,yl,cli)
'open 'infile2''
'set grid on'
'set xaxis 0 44 5'
'set vrange 'v1' 'v2''
'set ylint 'yl''

'set t 1 45'
'set ccolor 1'
'set cthick 10'
'set cstyle 1'
'set cmark 0'
'd hgt'

'set ccolor 2'
'set cthick 10'
'set cstyle 2'
'set cmark 0'
'define dummy='cli''
'd dummy'

'set ccolor 3'
'set cthick 10'
'set cstyle 2'
'set cmark 0'
'define dummy='cli*0.7071''
'd dummy'

'close 1'

'open 'infile1''
'set ccolor 1'
'set cthick 10'
'set cstyle 0'
'set cmark 2'
'set t 1 45'
'd hgt'

'close 1'



return
