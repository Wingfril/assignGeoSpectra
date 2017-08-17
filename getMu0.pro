pro getMu0, head_tmp, lat, lon, mu0 = mu0

    aucm   = 1.49598e13

    ; Parsing info from header    
    ra = drm_getfits(head_tmp, 'RA')
    aplan = ra

    dec = drm_getfits(head_tmp, 'DEC')
    dplan = dec

    ; Horizontal step size in arc sec
    delh = drm_getfits(head_tmp, 'PIXSCALE', status=status)
    if(status NE 0) then begin
        delh = drm_getfits(head_tmp, 'PLATE_SC')
    endif

    
    ; Vertical step size in arc sec
    delv= drm_getfits(head_tmp, 'PIXSCALE',status=status)
    if(status NE 0) then begin
        delv = drm_getfits(head_tmp, 'PLATE_SC')
    endif
    ; Distance of object from the earth
    gdist = drm_getfits(head_tmp,'DISTOBJ')
    ; heliocentric longitude
    hra = drm_getfits(head_tmp,'HLON')
    ras = float( (hra+180.0) mod 360 )
    ; heliocentric latitude
    hdec = drm_getfits(head_tmp,'HLAT')
    soldec = -hdec *!pi /180.
    ds = soldec


    ae =  aplan*(!pi/180.0)-!pi
    de = -dplan*(!pi/180.0) 
    object = drm_getfits(head_tmp,'OBJECT')
    object = strtrim(strlowcase(object),2)

     if object eq 'jupiter' or object eq 'saturn' then $
     lcm = drm_getfits(head_tmp,'LCMIII') $
 else $
     lcm = drm_getfits(head_tmp,'LCM')


    case 1 of
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
    endcase

    ax = recm/(gdist*aucm)*(180.0/!pi)*3600.0
    by = ax*(1.-oblat)
    ax = ax/delh    
    by = by/delv
    ae = ax
    ap = by
    o = ae/ap

    if ras lt 0.0 then ras = ras + 360.
    ;print,'ras ',ras
    ras = ras * !pi/180.	; convert to radians


    rae = float( (aplan+180.0) mod 360. )
    if rae lt 0.0 then rae = rae + 360.
    ;print,'rae ',rae
    rae = rae * !pi/180.	; convert to radians

lat *= !CONST.PI/ 180
lon = (lon  + lcm - 360)* !CONST.PI/ 180

        ; check output option, if not selected, skip it.
    czas = cos(atan(o*o*tan(lat)))*cos(ds)*cos(lon-(ras-rae)) + $
                  sin(atan(o*o*tan(lat)))*sin(ds)   
    if czas lt 0 then begin
        czas = 0
    endif
mu0 = ACOS(czas)
end
