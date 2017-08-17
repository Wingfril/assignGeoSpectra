;=============================================================================
;+
; drm_lcm.pro
;
; PURPOSE :
;
;  Function lcm which takes in the header of a FITS file (of Jupiter)     
;  and computes the LCMII and LCMIII by reading in the date and time      
;  observed from the header.                                              
;  Given the date, the program looks in a file called                     
;  lcmdata which has the format 'mmddyy lcmii  lcmiii'.  Right now,       
;  lcmdata has values for July 1,1994 through Aug 10, 1994.  More         
;  values can be added easily by following the above format and looking      
;  up the values in the nautical almanac. 
;
; CALLING SEQUENCE :
;
;  header = drm_lcm(header,cancelevent=cancelevent,group=group)
;
; ARGUMENTS
;  INPUT : header - the FITS header for which to look up the LCM
;   
;          group - the ID of the top base widget
;
;  OUTPUT : NONE
;
; KEYWORDS
;  INPUT : cancelevent - If the cancel button is hit then cancelevent
;                        is set equal to 'Yes' and all changes made to
;                        the header up till this point are lost.
;
;          auto - do not issue any questions unless there is an error.
;
;  OUTPUT : NONE
;
; RETURN : header - the updated FITS header.
;
; EXAMPLE :
;
;  header = drm_lcm(header,cancelevent=cancelevent)
;
; RESTRICTIONS:
;
;  As noted above, the file drm_lcmdata must be present.  If it does
;  not have the relevant data, the user can input it himself, but its
;  easier to have all the data in drm_lcmdata.
;
; PROCEDURES USED:
;       Procedures: drm_putfits,drm_util
;
; KNOWN BUGS : NONE
;
; ORIGINAL AUTHOR : T. Kanamori; 8/94
;
; UPDATE HISTORY : F. L. Wong; 7/95
;	--BMF--09/17/96--added check of object to allow for other planets
;				got rid of lcm data files, now calculated
;	--BMF--2/97 added all of the planets
;	
;	--JMF-- 3/15 added in date reading case for new spex header format. ln 95
;
;=============================================================================
;-
;--------------------
;Main function lcm  
;--------------------

function zmo_drm_lcm, head,cancelevent=cancelevent,group=group, auto=auto, $
   specfun=specfun

common lcmval,lcmii,lcmiii,header,lcii,lciii,htime

cancelevent='No'
if(NOT keyword_set(specfun)) then specfun=0

if(drm_exists()) then FITS_COMMENT=call_function('drm_acc_FITS_COMMENT') $
else FITS_COMMENT=''

header = head
yesno = 'Yes'

;-----------------------------------------------------
;Read in the data from the header passed in.;
;-----------------------------------------------------

   delta = drm_getfits(header,'DISTOBJ',status=c1,specfun=specfun)
   ra = drm_getfits(header,'RA',status=c2,specfun=specfun)
   dec = drm_getfits(header,'DEC',status=c3,specfun=specfun)
;print,c1,c2,c3
   if c1 or c2 or c3 then begin
	print,'Ephemeris information not present.  No LCMs calculated'
	return,header
   endif

   ;!================================== edit J. Fernandes 2015-03-20
   checkforzero = drm_getfits(header, 'DATE-OBS', specfun=specfun)
   if (checkforzero EQ 0) then begin
	;print, 'it"s broken, use new format'
	date=drm_getfits(header, 'DATE_OBS', specfun=specfun)
	;print, 'from drm_lcm, the date is   ', date
   time=drm_getfits(header, 'TIME_OBS', specfun=specfun)
   endif else begin
	;print, 'not broken, use old format'
   date=drm_getfits(header, 'DATE-OBS', specfun=specfun)
	;print, 'from drm_lcm, the date is   ', date
   time=drm_getfits(header, 'TIME-OBS', specfun=specfun)
   endelse
   ;!==================================
   object=drm_getfits(header, 'OBJECT', status=c, specfun=specfun)
   object = strtrim(strlowcase(object),2)
;   p = strpos(object,'jupiter')
;   if p ne -1 then xo = strmid(object,p,7)
;   if p eq -1 then begin
;   	p = strmid(object,'saturn',6)
;   	if p ne -1 then xo = strmid(object,p,6)
;   endif
;   if p eq -1 then xo = object
   ok = is_planet(object,planet=planet)
   if ok eq 1 then object = planet
;print,date,time,ra,dec,delta   
   case object of
        'mercury' : planetlcm,date,time,1,ra,dec,delta,lcm,lat,object=object
        'venus' : planetlcm,date,time,1,ra,dec,delta,lcm,lat,object=object
        'mars' : planetlcm,date,time,1,ra,dec,delta,lcm,lat,object=object
        'neptune' : planetlcm,date,time,1,ra,dec,delta,lcm,lat,object=object
        'uranus' : planetlcm,date,time,1,ra,dec,delta,lcm,lat,object=object
        'neptune' : planetlcm,date,time,1,ra,dec,delta,lcm,lat,object=object
	'jupiter' :  begin
		planetlcm,date,time,1,ra,dec,delta,long,lat,object=object
		lcmi = long

		planetlcm,date,time,2,ra,dec,delta,long,lat,object=object
		lcmii = long

		planetlcm,date,time,3,ra,dec,delta,long,lat,object=object
		lcmiii = long
	end
	'saturn' : begin
		planetlcm,date,time,3,ra,dec,delta,long,lat,object=object
		lcmiii = long
	end
	else	: return,header
    endcase

 ans = 'Yes'


if ans eq 'Yes' then BEGIN
  if object eq 'jupiter' or object eq 'saturn' then begin
      drm_putfits, header, 'LCMIII',lcmiii, $
		' System III longitude,degrees '+FITS_COMMENT, specfun=specfun
      if object eq 'jupiter' then begin
      	drm_putfits, header, $ 
   	  'LCMII',lcmii,' System II longitude,degrees '+FITS_COMMENT, $
	  specfun=specfun 
    	drm_putfits, header, $ 
   	  'LCMI',lcmi,' System I longitude,degrees '+FITS_COMMENT, $
	  specfun=specfun 
      endif
   endif else drm_putfits, header, $ 
   	  'LCM',lcm,' Central Meridian longitude,degrees '+FITS_COMMENT, $
	  specfun=specfun 
END 

return, header

end

