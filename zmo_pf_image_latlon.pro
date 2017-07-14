;=============================================================================
;+
; pf_image_latlon
;
; PURPOSE :  
;`
;  Given a the location (x,y) of a pixel in the an image,
; determine the planet latitude and longitude corresponding
; to that pixel.  The real work is done by routines in the
; planet libraries; this routine just gets the necessary
; data from the header and makes the calls.
; 
;'
; CALLING SEQUENCE :
;
;   status=pf_image_latlon(header, x, y, lat, long, hit, error)
;
;
; ARGUMENTS
;  INPUT : header - FITS header containing the appropriate ephemeris
;                   data.
;
;          x, y - Image coordinates of point.
;
;
;  OUTPUT : lat, long - Latitude and longitude corresponding to the 
;                       given image point.
;
;           hit - 1 if the pixel [x,y] is on the planet, 0 otherwise.
;
;           error - String possible error message if return status is
;                   nonzero.
;
;
;
; KEYWORDS 
;  INPUT : NONE
;
;  OUTPUT : NONE
;
;
;
; RETURN : status - 0 if ok, nonzero otherwise.
;
;
;
;
; EXAMPLE :
;
;
;
; KNOWN BUGS : NONE
;
;
;
; ORIGINAL AUTHOR : J. Spitale ; 8/94
;
; UPDATE HISTORY : 
;
;-
;=============================================================================
function zmo_pf_image_latlon, header,$                ; for ephemeris data
                          x, y, $                 ; input pixel location
                          lat, long, hit, $       ; output
                          error, a, b, c

 error=''

;---------------------get cx and cy---------------------------
 cx=drm_getfits(header, 'CX', status=s_1)
 cy=drm_getfits(header, 'CY', status=s_2)
 if(s_1 NE 0 OR s_2 NE 0) then $
  begin
   error=''
   return, '^Cannot find CX or CY fields in FITS header.'
  end


;------------------------set up camera---------------------
 status =  pf_setup_camera( header, $
                            planet_a, planet_b, planet_c, $
                            loc_cam, c0, c1, $
                            loc_planet, p0, p1, $
                            pix_radians)
 print, 'in pf_image_latlon : 1'

;planet_b *= b
;planet_a *= a
;planet_c *= c

print, 'planet_a, b, c'
print, planet_a
print, planet_b
print, planet_c

 if(keyword_set(status)) then return, status
 
 hit = pg_pix_2_latlon( loc_cam, c0, c1, $
                        loc_planet, p0, p1, $
                        planet_a,planet_b,planet_c, $
                        pix_radians, $
                        cx, cy, $
                        x, y, $
                        lat, long )
 print, 'in pf_image_latlon'
 print, 'lat'
 print, lat
 print, 'long'
 print, long
  return, 0
end
;=============================================================================.
