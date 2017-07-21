;=============================================================================
;+
; pf_get_mu
;
; PURPOSE :
;`
;  Computes the emmission cosine, mu, given a point [x,y], on an image,
; and a FITS header containing the appropriate ephemeris data.
;
;'
; CALLING SEQUENCE :
;
;   result=pf_get_mu(header, point)
;
;
; ARGUMENTS
;  INPUT : header - FITS header containing the appropriate ephemeris data.
;
;          point - Image coordinates, [x,y], of the point for which mu is
;                  to be computed.
;   
;
;  OUTPUT : NONE
;
;
;
; KEYWORDS 
;  INPUT : NONE
;
;  OUTPUT : lat,long - Latitude and longitude of the point on the planet.
;
;           sub_lat, sub_long - Latitude and longitude of the sub-earth
;                               point on the planet.
;
;           status - 0 if the header contains data pertaining to a planet,
;                    nonzero otherwise.
;
;
;
; RETURN : If the point is on the planet, then mu is returned, otherwise
;          -1 is returned.
;
;
;
;
; ORIGINAL AUTHOR : J. Spitale ; 6/95
;
; UPDATE HISTORY : 
;	Brendan M. Fisher--5/98--put in far edge of pixel calculations for
;		more accurate results near the limb, where pixels
;		(particularly large ones) subtend a large longitude range
;
;-
;=============================================================================
function pf_get_mu, header, point, lat=lat, long=long, $
   sub_lat=sub_lat, sub_long=sub_long, status=status

 status=0

 hit=0


;-----------get sub-earth lat/lon----------

 cx=drm_getfits(header, 'CX', status=status)
 if(status NE 0) then return, 0
 cy=drm_getfits(header, 'CY')

 stat=pf_image_latlon( header,$
                         cx, cy, $
                         lat0, long0, hit, $
                         error  )


;-----------get point lat/lon---------------

 stat=pf_image_latlon( header,$
                         point(0), point(1), $
                         lat, long, hit, $
                         error  )

; get lat/lon of far pixel edge
if hit then begin
    if long gt long0 then deltx = -1 else deltx = 1
    if lat lt lat0 then delty = -1 else delty = 1

    stat=pf_image_latlon( header,$
                         point(0)+deltx, point(1)+delty, $
                         latm, longm, hit2, $
                         error  )

;print,'Center ',lat0,long0
;print,'Point ',lat,long,abs(lat-lat0),abs(long-long0)

; check for off limb, if so, make longitude = lcm (+/-) 90
; don't know what limb was missed, so there probably is some amount
; of error at the north and south poles, but smaller than previously.
;
    if hit2 eq 0 then begin
        longm = long0 - deltx*90.
        latm = lat
    endif
;print,'Pointm ',latm,longm,abs(latm-lat0),abs(longm-long0)

;---------compute emission cosine, mu---------

; if(hit) then  begin
; use average lat and long.  Gives better results towards limb
   lat_rad=(lat+latm)/2.*!dtor
   long_rad=(long+longm)/2.*!dtor
   lat0_rad=lat0*!dtor
   long0_rad=long0*!dtor
   mu=n_round(cos(abs(lat0_rad-lat_rad))*cos(abs(long0_rad-long_rad)), 3)
   mu1=n_round(cos(abs(lat0_rad-(lat*!dtor)))*cos(abs(long0_rad-(long*!dtor))),3)
   mu_str=strmid(strtrim(mu, 2), 0, 5)
;print,mu,mu1
endif $
else mu=-1


 return, mu
end
;================================================================= 
