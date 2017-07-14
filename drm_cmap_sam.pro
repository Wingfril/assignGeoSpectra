;This is a DRM routine I modified to generate a list of latitudes/longitudes and the
;corresponding emission and incidence angles.
;
;It can be called directly in IDL through:
;drm_cmap_sam, img_path
;
;To generate the file needed by my plotting programs, one input is needed
;	img_path - path to the guide image 
;	
;The modified drm_cmap_sam routine calls read_map_image_sam instead of the original read_map_image.
;This is identical to the original but modified to call drm_map_image_sam instead of 
;drm_map_image, which is identical to the original routine but modified to call intp_sam 
;instead of intp. Intp_sam is very similar to the intp method in the original program, 
;but this outputs a CSV to "$guidimagepath.reftable". In retrospect I should have made the 
;extension something like "$guideimagepath_reftable.csv" so the file type is clear
;
;The output is a CSV containing:
;	latitude,longitude,mu,mu0
;
;The latitudes and longitudes generated are unfortunately not identical to those from 
;Greg's custom spatial extraction routine. So rather than looking up the emission and
;solar incidence angle based on the lat/lon given by that routine, you have to find
;the closest corresponding lat/lon within this reftable to the one you are interested in.
;My plot_2d program does this. There is a long list of latitudes/longitudes so it will
;take a while to sort through them all.
;


pro show_cyl,mapfile,numwin

;  Display a cylindrical map which has been written out by fortran unformatted ;write

ext = '.pix'
if(strpos(mapfile, '.pix') NE -1) then ext = ''

image = readfits(mapfile+ext,pixheader)
image=rotate(image,1)

mu = readfits(mapfile+ext,muheader)
mu=rotate(mu,1)

angle = where(mu lt 0.3)

image(angle) = 0.0

  wset, numwin
 
tvscl,rebin(image,4*180,4*90),/order

return
end




pro get_val,xs,ys,map,numwin, cm_msg_text

ans = 'y'
button = 0
wprint, cm_msg_text, 'Look at image and select a point - Click right button to end'

 wprint, cm_msg_text, 'Left button to select, Right button to quit.'

wset, numwin

tempmap = map
tempmap = rebin(map,180*4,90*4)

while (button ne 4) do begin
 cursor, x, y, /DOWN, /device
 button=!err


 xo=fix(x*180/!d.x_vsize) & yo=fix(y*90/!d.y_vsize)
 lat=-90.+2.*yo & long=(180-xo)*2.

 coord_str = '('+strtrim(string(lat),2)+','+strtrim(string(long),2)+')'+$
'   Pixel value: '+strtrim(string(tempmap(x,90*4-y)),2)

 wprint, cm_msg_text, coord_str
end

return
end



function poly_cyl, x
 
 u = abs(x)
 case 1 of
    (u lt 1.0) : poly_cyl = (1.5*u - 2.5)*u^2 + 1.0
    (u gt 1.0 and u lt 2.0) : poly_cyl = ((-0.5*u + 2.5)*u - 4.0)*u + 2.0
 else : poly_cyl = 0.0
 endcase
 
return, poly_cyl
end

pro choose_object, header

 common  funcdc, azxi,azyi,axxi,axyi,ayxi,ayyi,omga,tau,dtl,cx,cy, $
                u,v,nax1,nax2,phie,ae,ap,phij
 common  rotpole, apol1,dpol1
 common  ch_object, recm,oblat

; Choose which planet, in order to determine appropriate rotational pole

 recm  = 0.0  ; radius in cm
 oblat = 0.0  ; oblateness = (R_eq - R_pol) / R_eq
 omga  = 0.0  ; rotation rate (for doppler shift) radians per second (always positive?)
 apol1 = 0.0  ; ; north-pole right ascension
 dpol1 = 0.0  ; north-pole declination

 status=pl_get_params(drm_getfits(header,'OBJECT'), target_name=target_name)
 object = strlowcase(target_name)

; object = strlowcase(drm_getfits(header,'OBJECT'))
; BMF-03/2007 update to use get_ele like other programs, except still
; need radius, so put that in
; adjust radius values to those of the Astronomical Almanac
; Explanatory Suppliment (may update if SSD page comes back up)
 
 case 1 of
  (object eq 'mercury' or object eq 'mer') : begin
    recm=2.4397e8
    oblat=0.0
    omga=1.24001e-6;
    apol1=281.01
    dpol1= 61.45
  end
  (object eq 'venus' or object eq 'ven') : begin
    recm=6.0514e8
    recm=6.5019e8 ; BMF
    oblat=0.0
    omga=1.454e-5 ;(Assumes roughly 4-day rotation for atmospheric features)
    apol1=272.68
    dpol1= 67.21 
  end

  (object eq 'mars') : begin
    recm=3.3934e8
    recm=3.397e8 ; BMF
    oblat=0.0051865
    oblat=0.0065 ; BMF
    omga=7.088e-5
    omga=7.0702e-5 ; BMF
    apol1=317.61
    dpol1= 52.85
  end

  (object eq 'jupiter' or object eq 'jup') : begin
    recm=7.1398e9
    recm=7.1492e9  ; BMF
    oblat=0.06481
    oblat=0.06487 ; BMF
    omga=1.75853e-4 ; system III
    apol1=268.04
    dpol1= 64.49
  end

  (object eq 'saturn' or object eq 'sat') : begin
    recm=6.0000e9
    recm=6.0268e9 ; BMF
    oblat=0.107621
    oblat=0.09796 ; BMF
    omga=1.662e-4
    apol1= 40.23
    dpol1= 83.51
  end

 (object eq 'uranus' or object eq 'ura') : begin
    recm=2.5559e9
    oblat=0.022927
    omga=1.013e-4
    apol1=257.40
    dpol1=-15.18
  end

  (object eq 'neptune' or object eq 'nep') : begin
    recm=2.4764e9
    oblat=0.017081
    omga=1.083e-4
    apol1=299.42
    dpol1= 42.97
  end

  (object eq 'pluto' ) : begin
    recm=1.151e8
    oblat=0.0
    omga=1.13854e-5
    apol1=313.02
    dpol1= 9.09
  end


  else : begin
     msg=['Select which object is being observed :', $
          'V=Venus, M=Mars, J=Jupiter, S=Saturn, U=Uranus, N=Neptune']

     while (recm eq 0.0) do begin
        if(drm_exists()) then $
         yorn=drm_input_box(msg) $
        else $
         begin
          print, msg
          yorn='' & read,yorn
         end
        case 1 of
           (yorn eq 'V' or yorn eq 'v') : begin
  	      apol1=272.68
	      dpol1= 67.21
	      recm=6.0514e8
	      oblat=0.0
	      omga=0.0
           end
           (yorn eq 'M' or yorn eq 'm') : begin
              apol1=317.61
	      dpol1= 52.85
           end
           (yorn eq 'J' or yorn eq 'j') : begin
	      apol1=268.04
	      dpol1= 64.49
	      recm=7.1398e9
	      oblat=0.06481
	      omga=1.758e-4
           end
           (yorn eq 'S' or yorn eq 's') : begin
	      apol1= 40.14
	      dpol1= 83.50
           end
           (yorn eq 'U' or yorn eq 'u') : begin
              recm=2.5559e9
              oblat=0.022927
              omga=1.013e-4
              apol1=257.40
              dpol1=-15.18
           end
           (yorn eq 'N' or yorn eq 'n') : begin
              recm=2.4764e9
              oblat=0.017081
              omga=1.083e-4
              apol1=299.42
              dpol1= 42.97
           end

           else : begin
              print,'recm,oblat,omga,apol1,dpol1 are not'
              print,'hardwired into choose_object.pro'
           end
        endcase
     endwhile
    end
 endcase
date = drm_getfits(header,'DATE-OBS',status=status)
if(status NE 0) then begin
 date=drm_getfits(header, 'DATE_OBS')
 endif
typ = datetype(date)
case typ of
    0 : begin  ; date is 'dd/mm/yy'
        check = strtrim(date,2)
        p1 = strpos(check,'/',0)
        day = fix(strmid(check,0,p1))
        p2 = strpos(check,'/',p1+1)
        month = fix(strmid(check,p1+1,p2-(p1+1)))
        year = fix(strmid(check,p2+1,strlen(check)-(p2+1))) + 1900
    end
    1 : begin ; CCYY-MM-DD
        year = fix(strmid(date,0,4))
        month = fix(strmid(date,5,2))
        day = fix(strmid(date,8,2))
    end
    2 : begin   ;  CCYY-MM-DDThh:mm:ss[.sss...]
                ; ignor time string for this case
        year = fix(strmid(date,0,4))
        month = fix(strmid(date,5,2))
        day = fix(strmid(date,8,2))
        pos = strpos(date,'T') + 1
        hh = fix(strmid(date,pos(0),2))
        mm = fix(strmid(date,pos(0)+3,2))
        ss = float(strmid(date,pos(0)+6,strlen(date)))
        time = (hh+(mm+(ss)/60.D)/60.D)/24.D
    end
    else : return
endcase

sys = 3
get_ele,object,year,sys,a1,d1,w0,wt

apol1 = a1
dpol1 = d1
omga = (wt/180. * !pi)/86400.

return
end

;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
; 1993 Feb. 23:  Add code to treat case CCW=180.
;
; 1990 June 28:  Change READ for characters from image file to
;	  be direct access so program will work on
;	  Sparcstations - EQUIVALENCE of ch and idata
;	  changed to reflect this by starting data at 2881th byte.
;
; 1990 Aug. 3:	Change reads to get parameter information from image
;	  header instead of from shapedata file
;
; 1990 Sept. 4:  Generalize to add information for different planets
;	  Add conditional calibrator airmass = intial map airmass if
;	    calibration is not from a celestial source
;	  Add optional output of header and calibrated image
;
; 1991 July 15: Generalize to treat arbitrary CCW cases.
;
; 1993 July 31: Conversion to IDL and standard FITS header reading
;
; 1996 June 07: Modified to increase efficiency - Kartik C. Parija
;
; 1996 July 23: Converted to matrix operation, cleaned whole file of much
;		spurious stuff, added output options -- BMF
;------------------------------------------------------------------------------

;	This program computes a latitude-longitude to x-y table
;	  and reads the original image.
;	  It then interpolates on the image to create a new cylindrical-
;	  projection map and writes this map to a file.

;	Writes files:	1) intensity file (.pix)
;                       2) mu (.mu)
;                       3) mu0 [solar incidence angle] (.mu0)    {optional}
;			4) vdop - doppler velocity map (.vdop)   {optional}

;	 Read information from original file header:
;
;	  no. of columns in map, no. of rows, telescope name, object
;	  name, date, local time, right ascension, declination,
;	  U.T., air mass, horizontal step size, vertical step size,
;         rotation of image on sky (positive is clockwise),
;	  background level, uncertainty of background level,
;	  calibration scaling factor, uncertainty in calibration scaling
;         factor, heliocentric RA and DEC, distance of planet from sun,
;         distance from earth to planet, radial velocity, local central
;         meridian, and location of center of planet (cx,cy).
;
;------------------------------------------------------------------------------


function drm_map_image_sam, header,head_tmp,img_path

 common  ees, ezz,exx,eyy,exz,eyz,exy
 common  aas, azz,azx,azy,axz,axx,axy,ayz,ayx,ayy
 common  funcdc, azxi,azyi,axxi,axyi,ayxi,ayyi,omga,tau,dtl,cx,cy, $
                 u,v,nax1,nax2,phie,ae,ap,phij
 common  radec, soldec,rae,ras
 common  rotpole, apol1,dpol1
 common  yorns, yorn_interp, yorn_doppler, yorn_cosine
 common  ch_object, recm,oblat
 common  imagemosaic, image_mosaic
 common  h,head

@cmap_block.common

 get_fits, header,head_tmp,aplan,dplan,delh,delv,ccw,bg,sigbg, $
     calib,sigcal,hra,hdec,hdist,gdist,lcm,relvel,cx,cy,xoffset,yoffset
 PRINT, 'IN DRM_MAP_IMAGE_SAM, 5'
; Read location information
 u = 0
 v = 0

 aucm   = 1.49598e13

; Calculate TRUE semimajor and semiminor axes.
 ax = recm/(gdist*aucm)*(180.0/!pi)*3600.0
 by = ax*(1.-oblat)
 ax = ax/delh
 by = by/delv
 ae = ax
 ap = by
 ra2 = 1./(ax*ax)
 rb2 = 1./(by*by)
;print,'1',aplan,dplan,ccw
 thphi_cmap, aplan,dplan,theta,phi,ccw
 th = theta
;print,phi
; Translate the geometry to get RA and DEC as seen at planet.
 soldec = -hdec *!pi /180.
;print,'hra ',hra
ras = float( (hra+180.0) mod 360 )
if ras lt 0.0 then ras = ras + 360.
;print,'ras ',ras
ras = ras * !pi/180.	; convert to radians

;print,'aplan ', aplan
rae = float( (aplan+180.0) mod 360. )
if rae lt 0.0 then rae = rae + 360.
;print,'rae ',rae
rae = rae * !pi/180.	; convert to radians
 PRINT, 'IN DRM_MAP_IMAGE_SAM, 6'
; Generate the rotation between coordinate systems:

; 0. Define cosines,sines of Euler angles phi,th

 csp = cos(phi)
 snp = sin(phi)
 cst = cos(th)
 snt = sin(th)

; 1. Generate elements of Euler rotation matrix:

 azz = csp
 azx = snp
 azy = 0.0
 axz = -cst*snp
 axx = cst*csp
 axy = snt
 ayz = snt*snp
 ayx = -snt*csp
 ayy = cst

;    Transform sub-earth point to prime-coord. system

 zpe = azz
 xpe = axz
 ype = ayz

;    Find longitude of earth relative to this system

 zxmage = sqrt(zpe*zpe+xpe*xpe)
 elonp  = asin(xpe/zxmage)
 if (zpe lt 0.0) then elonp = !pi-elonp
 if ((xpe lt 0.0) and (zpe ge 0.0)) then elonp = 2.0*!pi+elonp
 xlone = elonp
 phie = xlone
;stop
; 2. Generate E's = coefficients of ellipsoid eqn in zxy system
 PRINT, 'IN DRM_MAP_IMAGE_SAM, 6.1'
 ezz = ra2*(azz*azz+axz*axz)+rb2*(ayz*ayz)
 exx = ra2*(azx*azx+axx*axx)+rb2*(ayx*ayx)
 eyy = ra2*(azy*azy+axy*axy)+rb2*(ayy*ayy)
 exz = ra2*(azz*azx+axz*axx)+rb2*(ayz*ayx)
 eyz = ra2*(azz*azy+axz*axy)+rb2*(ayz*ayy)
 exy = ra2*(azx*azy+axx*axy)+rb2*(ayx*ayy)


; intp - interpolation procedure to get x,y ordered pairs
;	out of set of latitudes, longitudes in SYSIII ....

; if (yorn_doppler eq '') then $
;    print,'Default nlat, nlon are assumed (defaults 90,180):'
labels = strarr(2)
typ = strarr(2)
init = strarr(2)
par1 = strarr(2)
par2 = strarr(2)
 PRINT, 'IN DRM_MAP_IMAGE_SAM, 6.2'
labels(0) = 'Choose a resolution: ' 
typ(0) = 'DROPLIST'
init(0) = '4x - 0.5  degree res.'
list = ['1x - 2.0  degree res.','2x - 1.0  degree res.', $
	'4x - 0.5  degree res.','8x - 0.25 degree res.']
;init(0) = list(2)
hand=handle_create()
handle_value,hand,list,/set
par1(0) = hand
par2(0) = ''

 PRINT, 'IN DRM_MAP_IMAGE_SAM, 6.3'
lam = drm_getfits(head,'LAMBDA')


labels(1) = 'Choose an output option: ' 
typ(1) = 'DROPLIST'
;init(1) ='-                  Cylmap and mu                 -'
list2 = ['-                  Cylmap and mu                 -',$
	 '-         Cylmap and mu and mu0          -',$
	 '- Cylmap and mu and mu0 and vdop -']
if lam lt 7.0 then init(1) = list2(1)
if lam ge 7.0 and lam lt 8.0 then init(1) = list2(2)
;if lam ge 8.0 and lam lt 13.0 then init(1) = list2(1)
;if lam ge 13.0 then init(1) = list2(0) 
if lam ge 8.0 then init(1) = list2(0) 
hand2=handle_create()
handle_value,hand2,list2,/set
par1(1) = hand2
par2(1) = ''
 PRINT, 'IN DRM_MAP_IMAGE_SAM, 6.4'
 status=drm_set_box( [  [labels(0),typ(0),init(0),par1(0),par2(0)], $
                        [labels(1),typ(1),init(1),par1(1),par2(1)] ], $
                        new_settings=new_settings)
if status eq -1 then return,-1
res = where(new_settings(0) eq list)
resolution = res(0)
opt = where(new_settings(1) eq list2)
option = opt(0)
 PRINT, 'IN DRM_MAP_IMAGE_SAM, 6.5'
;print,resolution,option

;old way
;resolution=drm_list_box($
;	       ['        Choose a resolution:',$  
;		'WARNING: higher resolutions take longer', $
;		'        (by square of x factor)'],$	
;		['standard - 1x - 2.0  degree res.',$ 
;		 '           2x - 1.0  degree res.',$  
;		 '           4x - 0.5  degree res.',$  
;		 '           8x - 0.25 degree res.']) 
CASE resolution OF 	
       -1:	RETURN, 0
	0:	resolution=1	
	1:	resolution=2	
	2:	resolution=4	
	3:	resolution=8
ENDCASE			
;help,resolution
; old way		
;option=drm_list_box(['Choose an output option:'],$	
;		['Cylmap and mu',$ 		; option = 0
;		 'Cylmap and mu and mu0',$  	; option = 1
;		 'Cylmap and mu and mu0 and vdop']) 	; option = 2
;print,resolution,option
if option eq -1 then return,0
;--BMF--I have used tau, and unused common, to pass the option around where
;	it is needed
;print,resolution,option
tau = option 	
;stop
widget_control,/hourglass
;
;
 nlat = 90*resolution		
 nlon = 180*resolution		
 lcm0 = lcm
 PRINT, 'IN DRM_MAP_IMAGE_SAM, 6.6'
; INTP returns arrays x,y,cza,czas, all of which are 
; 90*resolution+1x180*resolution reals.

;     (convert lcm0 to radians)
 lcm0 = lcm0*!pi/180.0

 intp_sam, nlat,nlon,lcm0,x,y,cza,czas,vdop,gdist,relvel,img_path,lcm
  PRINT, 'IN DRM_MAP_IMAGE_SAM, 6.7'
 ;GF testing lonlat
 ;stop
 ;lonlat,(273-cx),(187-cy),phie,gflat,gflon,gfczae
 ;print,'lat',gflat
 ;print,'lon',gflon
 ;done

; (convert lcm0 back to degrees)
 lcm0 = lcm0*180.0/!pi
PRINT, 'MAP_IMAGE_SAM, 6"
drm_msg_box,['Interpolating...'], $
		text_id=textid, /no_button, $
                 /no_beep, id=msg_id

 b = fltarr(nlat,nlon)	
 ;b(*,*) = 0.0

 ccwr = -ccw*!pi/180.0
 cosccw = cos(ccwr)
 sinccw = sin(ccwr)
;--BMF--incorporated xxx and yyy into xx and yy lines to save memory and
;	increase speed.
; xxx = (x-cx)*cosccw-sinccw*(y-cy)
; yyy = (x-cx)*sinccw+cosccw*(y-cy)
 xx = ((x-cx)*cosccw-sinccw*(y-cy))+cx
 yy = nax2-((x-cx)*sinccw+cosccw*(y-cy))-cy

; b(*,*)=0.0

 b=bilinear(image_mosaic,xx,yy)
 ng = where(xx lt 0.0 or yy lt 0.0 or xx gt nax1-1 or yy gt nax2-1)
 if ng(0) ne -1 then $
    b(ng) = 0.
; b((where(xx lt 0.0 or yy lt 0.0 or xx gt nax1-1 or yy gt nax2-1))) = 0.

;------------------------------------------------------------------------------

;Display cylindrical map.



;--BMF--comented out, not used
;nebn=14.
;nebs=7.
;sebn=-2.
;sebs=-18.
;latn=(90.-nebn)/2.		;?????????????????????????????????????????
;lats=(90.-nebs)/2.
;latsn=(90.-sebn)/2.
;latss=(90.-sebs)/2.

;b = rotate(b,1)
;mu = rotate(cza,1)
;minmu=0.3
;image=b * (mu gt minmu)
;image=b

widget_control, msg_id, /destroy

return, rotate(b,1)
end

;------------------------------------------------------------------------------

pro thphi_cmap, aplan,dplan,theta,phi,ccw

; This routine takes RA,DEC of Jupiter at observation time
; and returns Euler rotation angles theta,phi which relate
; sky-coordinate system to coordinate system on Jupiter with
; yp-axis aligned along rotation axis. Note third Euler angle
; is assumed zero!

; Common for rotational pole orientation for the planet
 common  rotpole, apol1,dpol1

; aplan = planetary RA  (degrees)
; dplan = planetary declination  (degrees)
; apol  = RA  of rotation pole
; dpol  = DEC of rotation pole

; subscript e refers to sub-earth point
; subscript pol refers to rotation axis
; subscript y refers to NP on sky-plane

; a.. refers to Right Ascension
; d... refers to declination

; A. Convert apol,dpol to radians:

 apol = apol1*(!pi/180.0)
 dpol = dpol1*(!pi/180.0)

; B. Find RA ,DEC of sub-earth point:

 ae =  aplan*(!pi/180.0)-!pi
 de = -dplan*(!pi/180.0)

; C. Find RA DEC of y-axis

 if (de le 0.0) then begin
   ay = ae
   dy = de+0.5*!pi
 endif else begin
   ay = ae-!pi
   dy = 0.5*!pi-de
 endelse
 if (ccw eq 180.0) then begin
   ay = ay+!pi
   dy = -dy
 endif

; D. Find theta: cos(theta) = sin(dpol)sin(dy)+cos(dpol)cos(dy)*
;	cos(apol-ay)

 theta = sin(dpol)*sin(dy)+cos(dpol)*cos(dy)*cos(apol-ay)
 theta = acos(theta)

; E. Find cartesian coordinates of points y,e,pole(=n)

 xy = cos(dy)*cos(ay)
 yy = cos(dy)*sin(ay)
 zy = sin(dy)
 xn = cos(dpol)*cos(apol)
 yn = cos(dpol)*sin(apol)
 zn = sin(dpol)
 xe = cos(de)*cos(ae)
 ye = cos(de)*sin(ae)
 ze = sin(de)

; F. Find components of cross-product of y X n and make unit vector

 x1 = yy*zn-zy*yn
 y1 = xn*zy-xy*zn
 z1 = xy*yn-xn*yy

 xmag = sqrt(x1*x1+y1*y1+z1*z1)
 xmag = 1./xmag

 xzp = x1*xmag
 yzp = y1*xmag
 zzp = z1*xmag

; G. Find components of e X zp, and |e X zp|

 x0 = ye*zzp-ze*yzp
 y0 = ze*xzp-xe*zzp
 z0 = xe*yzp-ye*xzp

 ezmag = sqrt(x0*x0+y0*y0+z0*z0)

; H. Find  phi, where here phi between 0 and pi

 phi = xe*xzp+ye*yzp+ze*zzp
; print,'phi',phi
 if phi lt 0. then phi = (phi > (-1.)) else phi = (phi < 1.)
 phi = acos(phi)
; print,'acos(phi)',phi
; stop

; I. Find S= dot product of y on (x0,y0,z0)
;     if S>=0, 0<=phi<pi. if S<0 pi<=phi<2*pi

 s = xy*x0 + yy*y0 +zy*z0
 if (s lt 0) then phi = 2.0*!pi-phi

return
end

;------------------------------------------------------------------------------

pro lonlat, x,y,xlone,xlat,xlon,czae

 common  ees, ezz,exx,eyy,exz,eyz,exy
 common  aas, azz,azx,azy,axz,axx,axy,ayz,ayx,ayy
 
;  Given point x,y in sky coordinates with origin centered
;  on planet, find latitude,longitude of x,y or return value
;  of 1000 if point not on planet.

; 3. Find z(x,y):

 az = ezz
 bz = 2.*(x*exz + y*eyz)
 cz = exx*x^2 + 2.*x*y*exy + eyy*y^2 - 1.

 z1 = bz/(2.*az)
 z2 = cz/az
 z3 = z1^2 - z2

len = n_elements(z3)

;   if z3 less than 0 then point x,y not on planet

g= where(z3 gt 0.,gcount)
ng = where(z3 le 0.,ngcount)  ;ajf, 10/4/96

;--BMF--superseceeded by where command

; if (z3 gt 0.0) then begin

z = fltarr(len,/nozero)

    z(g) = -z1(g) + sqrt(z3(g))

; 4. Find normal to ellipsoid at (z,x,y) = gradient

gz = fltarr(len,/nozero)
gx = fltarr(len,/nozero)
gy = fltarr(len,/nozero)

    gz(g)=2.*(ezz*z(g)+exz*x(g)+eyz*y(g))
    gx(g)=2.*(exx*x(g)+exz*z(g)+exy*y(g))
    gy(g)=2.*(eyy*y(g)+eyz*z(g)+exy*x(g))

; 5. Find cos(zenith angle) of earth
gmag = fltarr(len,/nozero)
czae = fltarr(len)

    gmag(g)=sqrt((gz(g))^2+(gx(g))^2+(gy(g))^2)
    czae(g)=gz(g)/gmag(g)


;--BMF--conserve memory for increased speed
gz = 0
gx = 0
gy = 0
gmag = 0


;   A. Transform P(z,x,y) to prime-coord. system zp,xp,yp
zp = fltarr(len,/nozero)
xp = fltarr(len,/nozero)
yp = fltarr(len,/nozero)

    zp(g) = azz*z(g)+azx*x(g)+azy*y(g)
    xp(g) = axz*z(g)+axx*x(g)+axy*y(g)
    yp(g) = ayz*z(g)+ayx*x(g)+ayy*y(g)

;  Find latitude

r = fltarr(len,/nozero)
xlat = fltarr(len)

   r(g)    = sqrt(zp(g)*zp(g)+xp(g)*xp(g)+yp(g)*yp(g))
   xlat(g) = asin(yp(g)/r(g))

;--BMF--conserve memory for increased speed
r = 0

;  Find longitude relative to zp-axis
zxmag = fltarr(len,/nozero)
xlon = fltarr(len)

   zxmag(g) = sqrt((zp(g))^2+(xp(g))^2)
   xlon(g)  = asin(xp(g)/zxmag(g))

;--BMF--conserve memory for increased speed
zmag = 0

;   if (zp lt 0.0) then xlon = !pi-xlon
gg = where(zp(g) lt 0.)
if gg(0) ne -1 then xlon(g(gg)) = !pi-xlon(g(gg))

;   if ((xp lt 0.0) and (zp ge 0.0)) then xlon = 2.0*!pi+xlon

gg = where((xp(g) lt 0.) and (zp(g) ge 0.))
if gg(0) ne -1 then xlon(g(gg)) = 2.*!pi+xlon(g(gg))


;  Find longitude relative to central meridian

   xlon(g) = xlon(g)-xlone
;print,max(xlat)
;print,max(xlon)

; endif else begin
if ngcount ne 0 then begin
    xlat(ng) = 1000.0
    xlon(ng) = 1000.0
endif
;    czae(ng) = 0.
; endelse
;wset,0
;disp,xlat,1
;disp,xlon,1,y=100

return 
end

;------------------------------------------------------------------------------

pro intp_sam, nlat,nlon,lcm0,x,y,cza,czas,vdop,gdist,vel,imgpath,lcm

; Given grid of latitudes, longitudes in System III, output set
; of pixel (x,y) pairs for each grid point to use in interpolation from map
;--BMF--more consistent notation common declaration
 common  aas, azz,azx,azy,axz,axx,axy,ayz,ayx,ayy
 common  funcdc, azxi,azyi,axxi,axyi,ayxi,ayyi,omga,tau,dtl,cx,cy, $
                u,v,nax1,nax2,phie,ae,ap,phij
 common  radec, soldec,rae,ras

nthtime = 0 & thsndtime = 0 
res = FIX(nlat/90.)
nthou = 8*(res^2)
drm_msg_box,['Creating coordinate map...'], $
		text_id=textid, /no_button, $
                 /no_beep, id=msg_id

 npmx = 180 & nlmx = 90	;
 aucm = 1.49598e13
;--BMF--conserve memory for increased speed
;long3 = fltarr(nlon)
;lat = fltarr(nlat,/nozero) 
;r = fltarr(nlat,/nozero) 
;yp = fltarr(nlat,/nozero) ;**
;rc = fltarr(nlat,/nozero) 
; czal = fltarr(nlat,nlon) 
 zppyn = fltarr(nlat,nlon)	;****************************
 x    = fltarr(nlat,nlon) 
 y    = fltarr(nlat,nlon) 
 vdop = fltarr(nlat,nlon) 
; Set up an array of latitudes and longitudes in the sysIII system:
;    1. latitudes run from 90-(180/nlat) to just short of -90 in
;	 180/nlat steps
;    2. longitudes run from 0 to just short of 360, spaced equally
;        by 360/nlon steps

; (latitudes and longitudes must be in radians)
;-note:these are vectors
 lat  = -findgen(nlat)*(!pi/nlat) + !pi/2.0
 lon3 =  findgen(nlon)*(2.0*!pi/nlon)

;  Find r(i) = radius of ellipsoid from origin to point at latitude i
;	and yp(i) = r(i)*sin(lat), rc(i)=r(i)*cos(lat(i))

 o = ae/ap
 ds = soldec
;--BMF--removed loop
; for i = 0,nlat-1 do begin
;--BMF--removed use of rtemp and rtemp1 for increased speed and memory cons.
;    rtemp  = ae*sin(lat)
;    rtemp1 = ap*cos(lat)
;    rtemp  = rtemp^2+rtemp1^2
    r   = sqrt((ae^2*ap^2)/((ae*sin(lat))^2 + (ap*cos(lat))^2))
;--BMF--superceeded by matrix notation code
;    yp  = r*sin(lat)
;    rc  = r*cos(lat)
; endfor
;-note: vector
    phase = lon3-lcm0-phie

 itrubno = 0

; Loop thru latitudes (outer loop) and longitudes (inner loop)

;--BMF--removed-no longer used by matrix notation
;    azxr = azx*rc
;    axxr = axx*rc
;    ayxr = ayx*yp
;    azyr = azy*rc
;    axyr = axy*rc
;    ayyr = ayy*yp
;
;--BMF--matrix operations instead of loops
;
; create matracies of lat and lon, and r values
latr = fltarr(nlat,nlon,/nozero)
lonr = fltarr(nlat,nlon,/nozero)
rr   = fltarr(nlat,nlon,/nozero)

; use IDL trick with row and column vectors
;for j=0,nlon-1 do latr(*,j) = lat
for j=0,nlon-1 do latr(0,j) = lat

col_phase = fltarr(1,n_elements(phase))
col_phase(0,*) = phase
;for i=0,nlat-1 do lonr(i,*) = phase
for i=0,nlat-1 do lonr(i,0) = col_phase

;for j=0,nlon-1 do rr(*,j) = r
for j=0,nlon-1 do rr(0,j) = r
;stop
;--BMF--conserve memory for increased speed
lat = 0
phase = 0
lon3=0
r = 0 

; find lat's and lons that are visible
zppyn =  azz*rr*cos(latr)*cos(lonr)$
	-axz*rr*cos(latr)*sin(lonr)$
	+ayz*rr*sin(latr)

g = where(zppyn gt 0.)   ; good (visible) ones, calculate for these only
;if g(0) eq -1 then begin
;    print,'Error in c-map'
;    stop
;endif
zppyn = 0

; x and y contain the x and y coords for each lat and lon
x(g) = cx+azx*rr(g)*cos(latr(g))*cos(lonr(g)) $
         -axx*rr(g)*cos(latr(g))*sin(lonr(g))   $
         +ayx*rr(g)*sin(latr(g))

y(g) = cy+azy*rr(g)*cos(latr(g))*cos(lonr(g)) $
         -axy*rr(g)*cos(latr(g))*sin(lonr(g))   $
         +ayy*rr(g)*sin(latr(g))

;print,'tau=',tau

if tau eq 2 then $ 	; check output option, if not selected, skip it.
    vdop(g) = omga*gdist*aucm*(!pi/180.)/3600.* $
	     (azz*rr(g)*cos(latr(g))*sin(lonr(g)) + $
	      axz*rr(g)*cos(latr(g))*cos(lonr(g)))*1.e-5 + vel 

;--BMF--conserve memory for increased speed
rr = 0


lonlat,(x(g)-cx),(y(g)-cy),phie,xlat1,xlon1,czal 

cza  = fltarr(nlat,nlon)   ; cza is mu

cza(g) = czal
czal = 0
; check for illegal values--should be caught by zppyn above, but not always
ng = where(cza lt 0. or cza gt 1.0)
if ng(0) ne -1 then cza(ng) = 0.
ng = 0
latr(g) = xlat1
xlat1 = 0
lonr(g) = xlon1
xlon1 = 0

;xlatg(g) = atan(o*o*tan(latr(g)))

czas = fltarr(nlat,nlon)  ; czas is mu0

if tau ge 1 then $   ; check output option, if not selected, skip it.
    czas(g) = cos(atan(o*o*tan(latr(g))))*cos(ds)*cos(lonr(g)-(ras-rae)) + $
              sin(atan(o*o*tan(latr(g))))*sin(ds)
; check for illegal values--should be caught by zppyn above, but not always
ng = where(czas lt 0. or czas gt 1.0)
if ng(0) ne -1 then czas(ng) = 0.
ng = 0

fname=imgpath+'.reftable'
OPENW,31,fname

csvindex=1.0
csvlats = fltarr(259200,1)
csvlons = fltarr(259200,1)
csvmus = fltarr(259200,1)
csvmu0s = fltarr(259200,1)
res=2.
lonpix=res*(360.-lcm+lonr*180./!pi)
latpix=res*(90.+latr*180./!pi)
lonpix[where(lonr eq 1000 or latr eq 1000)] = 0
latpix[where(lonr eq 1000 or latr eq 1000)] = 0
lon=360.-lcm+lonr*180./!pi
lat=latr*180./!pi
lon[where(lonr eq 1000 or latr eq 1000)] = !values.f_nan
lat[where(lonr eq 1000 or latr eq 1000)] = !values.f_nan
print,'Writing to CSV...'
counti=1
help,latr
while counti lt 360 do begin	
	countj=1
	while countj lt 720 do begin
		if ( abs( lat[counti,countj] ) le 90 ) && $
		( lat[counti,countj] ne 0 ) && $
		(lon[counti,countj] le 360) && $
		(lon[counti,countj] gt 0) then begin
			csvlats[csvindex]=lat[counti,countj]
			csvlons[csvindex]=lon[counti,countj]
			csvmus[csvindex]=acos(cza[counti,countj])
			csvmu0s[csvindex]=acos(czas[counti,countj])
        PRINT, CZA[COUNTI, COUNTJ]
			csvindex=csvindex+1.0	
		endif 
		countj=countj+1
	endwhile
	counti=counti+1
endwhile
nzcsvlats = fltarr(csvindex,1)
nzcsvlons = fltarr(csvindex,1)
nzcsvmus = fltarr(csvindex,1)
nzcsvmu0s = fltarr(csvindex,1)
counti=1.0
while (counti lt csvindex) do begin
	nzcsvlats[counti]=csvlats[counti]
	nzcsvlons[counti]=csvlons[counti]
	nzcsvmus[counti]=csvmus[counti]
	nzcsvmu0s[counti]=csvmu0s[counti]
	counti=counti+1.0
endwhile 
WRITE_CSV,fname,nzcsvlats,nzcsvlons,nzcsvmus,nzcsvmu0s
csvlats=0
csvlons=0
csvmus=0
csvmu0s=0
nzcsvlats=0
nzcsvlons=0
nzcsvmus=0
nzcsvmu0s=0
CLOSE,31

;--BMF--conserve memory for increased speed
latr =0
lonr = 0

; old code supersceeded by matrix stuff above
; left here for purposes of clarity of method 


goto, skip

for i = 0,nlat-1 do begin
    azxi = azxr(i)
    axxi = axxr(i)
    ayxi = ayxr(i)
    azyi = azyr(i)
    axyi = axyr(i)
    ayyi = ayyr(i)
    rci = rc(i)
    ypi = yp(i)
    for j = 0,nlon-1 do begin
   	phasej= phase(j)

; Evaluate time with u,v,omga = 0, which is approximate elapsed time to
; measurement of non-rotating point at latitude, longitude (i,j)

;--BMF--phij is used nowhere--remove next line
;       phij = phas0
       flag = 1
;--BMF--removed next two lines as not used
;       ftx = cx+azxi*cos(phas0)-axxi*sin(phas0)+ayxi-1.0
;       fty = cy+azyi*cos(phas0)-axyi*sin(phas0)+ayyi
;       if (ftx lt 0.0 or ftx gt float(nax2-1)) then flag = 0
;       if (fty lt 0.0 or fty ge float(nax1-2)) then flag = 0
;
;--BMF--no purpose-removed
;       ft2 = 0.0

; Estimate, for each latitude, which sysIII longs
; are visible on disk at time they are measured
; In following loop, sign of zpp is checked for each lat,long, then
; that latitude gets a 1 if zpp>0, a 0 if zpp<=0.

       zp0   = rci*cos(phasej)
       xp0   = -rci*sin(phasej)
       yp0   = ypi
       zpp0  = azz*zp0+axz*xp0+ayz*yp0

;       if (zpp0 gt 0.0 and flag eq 1) then begin
;          zppyn(i,j) = 1.0
;       endif else begin
;          zppyn(i,j) = 0.0
;       endelse


       if (zpp0 gt 0.0) then begin
;          phase  = phas0
          x(i,j) = cx+azxi*cos(phasej)-axxi*sin(phasej)+ayxi
          y(i,j) = cy+azyi*cos(phasej)-axyi*sin(phasej)+ayyi

	  xprim = -rci*sin(phasej)*gdist*aucm*(!pi/180.0)/3600.0
	  zprim =  rci*cos(phasej)*gdist*aucm*(!pi/180.0)/3600.0
	  vdop(i,j) = omga*(-azz*xprim+axz*zprim)*1.e-5 + vel
	  xpp = x(i,j) - cx
	  ypp = y(i,j) - cy
	  
	  nthtime = nthtime+1
          if ((nthtime/1000.)-fix(nthtime/1000.) eq 0) then BEGIN
	      thsndtime = thsndtime+1
              wprint,textid,['cylmap in progress...']
	  END

	  lonlat, xpp,ypp,phie,xlat,xlon,czae
	  cza(i,j) = czae
	  xlatg = atan(o*o*tan(xlat))

	  czas(i,j) = cos(xlatg)*cos(ds)*cos(xlon-(ras-rae)) + $
              sin(xlatg)*sin(ds)

      endif ; else begin
;
;	  x(i,j) = 0.0
;	  y(i,j) = 0.0
;	  cza(i,j)  = 0.0
;	  czas(i,j) = 0.0
;	  vdop(i,j) = 0.0
;
;      endelse
;
    endfor
 endfor

skip:

widget_control, msg_id, /destroy
return
end

;------------------------------------------------------------------------------

function read_map_image_sam, image_tiny,head_tmp,header,k,img_path
 
 common  funcdc, azxi,azyi,axxi,axyi,ayxi,ayyi,omga,tau,dtl,cx,cy, $
                u,v,nax1,nax2,phie,ae,ap,phij
 common  imagemosaic, image_mosaic
 common  h,head
;   Read tiles of images, offsets and FITS header
 
 nax1 = drm_getfits(header,'NAXIS1')
 nax2 = drm_getfits(header,'NAXIS2')

 inx = drm_getfits(head_tmp,'NAXIS1')
 iny = drm_getfits(head_tmp,'NAXIS2')
PRINT, 'READ_MAP_IMAGE, 3"
; help,image_tiny
 s = size(image_tiny)
 if inx ne s(1) or iny ne s(2) then begin
    print,'WARNING: Image and header info do not match.  Using image sizes.'
    nax1 = s(1)
    inx  = s(1)
    nax2 = s(2)
    iny  = s(2)
 endif 
 choose_object, head_tmp

; We don't normally use this in the sense concieved here
; (i.e. multiple images in one plane, with offsets to thier
; origins, and DRM does not call this routine with k!=0
; This is some old code....
; BMF Feb. 2007
 xoffset = sxpar(head_tmp,'XOFFSET')
 yoffset = sxpar(head_tmp,'YOFFSET')
; set them to zero to fix problem of negative index when
; xoffset is non-zero and nax1 = inx1 
; This was happening with some Gemini/South TReCS data that
; use XOFFSET and YOFFSET in the header for some other purpose
; than that above
; BMF Feb. 2007
 xoffset = 0
 yoffset = 0

 xoffset = nax1-fix(xoffset)-inx
 yoffset = fix(yoffset)
 
; print,'x,yoff',xoffset,yoffset,nax1,inx,nax2,iny,s

 if (k eq 0) then image_mosaic = fltarr(nax1,nax2) 
;stop
 image_mosaic(xoffset:xoffset+inx-1,yoffset:yoffset+iny-1) = $
     image_tiny(0:inx-1,0:iny-1)
PRINT, 'READ_MAP_IMAGE, 4"
 cmap_image=drm_map_image_sam(header,head_tmp,img_path)

return, cmap_image
end

;------------------------------------------------------------------------------

pro get_fits, header,head_tmp, aplan,dplan,delh,delv,ccw,bg,sigbg, $
        calib,sigcal,hra,hdec,hdist,gdist,lcm,relvel,cx,cy,xoffset,yoffset

 naxis1 = sxpar(header,'NAXIS1')
 naxis2 = sxpar(header,'NAXIS2')

ra = drm_getfits(head_tmp, 'RA')

aplan = ra

dec = drm_getfits(head_tmp, 'DEC')

dplan = dec

; horizontal step size in arc sec
 delh   = drm_getfits(head_tmp, 'PIXSCALE', status=status)
if(status NE 0) then begin
 delh=drm_getfits(head_tmp, 'PLATE_SC')
 endif
; vertical step size in arc sec
 delv   = drm_getfits(head_tmp, 'PIXSCALE',status=status)
if(status NE 0) then begin
 delv=drm_getfits(head_tmp, 'PLATE_SC')
 endif
; Counter-clockwise rotation of image from north
 ccw    = drm_getfits(head_tmp,'CCW')
;--BMF--3/97--DRM gives ccw in degrees COUNTER-clockwise from north, whereas
;	this program assume CLOCKWISE rotation is positive.
; 	So invert to match.  Either change sign, or reflect about 360.
 ccw = -ccw
; background (BZERO)
 bg    = drm_getfits(head_tmp,'BZERO')
; sigbg; uncertainty in BZERO
 sigbg = double(0.0)
; intensity of scale (BSCALE)
 calib = drm_getfits(head_tmp,'BSCALE')
 if calib eq 0 then calib = 1.
; its uncertainty
 sigcalib = double(0.0)
; heliocentric longitude
 hra   = drm_getfits(head_tmp,'HLON')
; heliocentric latitude
 hdec  = drm_getfits(head_tmp,'HLAT')
; heliocentric distance
 hdist = drm_getfits(head_tmp,'HDIST')
; distance of object from the earth
 gdist = drm_getfits(head_tmp,'DISTOBJ')
; local central meridian
 object = drm_getfits(head_tmp,'OBJECT')
 object = strtrim(strlowcase(object),2)
 if object eq 'jupiter' or object eq 'saturn' then $
     lcm = drm_getfits(head_tmp,'LCMIII') $
 else $
     lcm = drm_getfits(head_tmp,'LCM')
; radial velocity
 relvel = drm_getfits(head_tmp,'RVEL')
; center of planet location in x
 cx     = drm_getfits(head_tmp,'CX')
; center of planet location in y
 cy     = drm_getfits(head_tmp,'CY')

return
end

;------------------------------------------------------------------------------


;---------------------------------------------------------------------
pro drm_cmap_sam, img_path
@cmap_block.common

   image_tiny=readfits(img_path,header,/SILENT)
head_tmp = header
   _it = rotate(image_tiny, 7)  ; expect image to be oriented properly
    _h = header
    _ht = head_tmp

 latmax = 90 & lonmax = 180

; x = fltarr(latmax,lonmax) & y = fltarr(latmax,lonmax)
; vdop = fltarr(latmax,lonmax) & cza = fltarr(latmax,lonmax)
; czas = fltarr(latmax,lonmax)

 common  ees, ezz,exx,eyy,exz,eyz,exy
 common  aas, azz,azx,azy,axz,axx,axy,ayz,ayx,ayy
 common  funcdc, azxi,azyi,axxi,axyi,ayxi,ayyi,omga,tau,dtl,cx,cy, $
                u,v,nax1,nax2,phie,ae,ap,phij
 common  radec, soldec,rae,ras
 common  rotpole, apol1,dpol1
 common  yorns, yorn_interp, yorn_doppler, yorn_cosine
 common  h,head
PRINT, 'IN DRM_CMAP_SAM, 1"
head = header
k=0
   yorn_interp = 'blank'  &  yorn_doppler = 'blank'  &  yorn_cosine = 'blank'

yorn_interp = 'n'

;---------- generate the cmap ------------

cmap_out=read_map_image_sam( _it,_ht,_h,0,img_path)
PRINT, 'IN DRM_CMAP_SAM, 2"
if n_elements(cmap_out) ne 1 then begin
  mu_out=rotate(cza,1)
  mu0_out=rotate(czas,1)
  vdop_out=rotate(vdop,1)
endif
return
end

;------------------------------------------------------------------------------
