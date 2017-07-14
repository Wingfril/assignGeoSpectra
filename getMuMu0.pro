; 
; This procedure is called from dataCubeManipulation and it uses the geometry 
; csv from the guide image to match lon/lat with the lon/lat from each point
; on the spectra. After we get the index of the associated lon/lat,
; we can also get the emission angle and solar incidence angle ( mu and mu0)
; Quite a bit of this code is from Sam Hedges's plot2D.pro
;
pro getMuMu0, lonArray, latArray, refPath, muArray = muArray, $
              mu0Array = mu0Array
    refPath = refPath + '.reftable'
    geometryInfo = read_csv(refPath, HEADER = geometryHeader, $
                   N_TABLE_HEADER = 4, TABLE_HEADER = geometryTableHeader)
    latList = geometryInfo.field1
    lonList = geometryInfo.field2
    muList  = geometryInfo.field3
    mu0List = geometryInfo.field4

    arraySize = SIZE(lonArray, /DIMENSIONS)
    muArray = fltarr(arraySize[0])
    mu0Array = fltarr(arraySize[0])

    ;print, 'arraySize[0]-1'
    ;print, arraySize[0]-1
    help, lonArray
    help, latArray
    
    for i = 0, arraySize[0]-1 do begin
        ;Within all of the latitudes, locate the value closest to the 
        ;ap's latitude
        bin = VALUE_LOCATE(latList,latArray[i])
        ;If it is lower than the first latitude from the cmap, set to 
        ;the lowest value 
	    IF (bin eq -1) then begin
		    closest = latList[0]	
		    print,'min'
	    ENDIF ELSE BEGIN
            ; if it is greater than the greatest lat from the cmap, 
            ; set to the greatest
	        IF (bin eq (N_ELEMENTS(latList)-1)) then begin 
			    closest = latList[N_ELEMENTS(latList)-1]
			    print,'max'
		    ENDIF ELSE BEGIN 	
                ; It is between two values from the cmap, 
                ; so we need to determine which value is closer.
                ; To do this find the the difference between the ap. lat. 
                ; and the cmap lat on either side, which is less.
		        IF ( ABS(latList[bin] - latArray[i]) GT $
                     ABS(latList[bin+1] - latArray[i]) ) THEN BEGIN
                            ; closest to the value greater than the ap. lat
                   			closest = latList[bin+1]  	
			    ENDIF ELSE BEGIN
                            ; closest to the value less than the ap. lat.
						    closest = latList[bin]			
			    ENDELSE
		    ENDELSE
	    ENDELSE	
        ; Store the indices containing the closest latitude 
        ; in the rightlatindices variable
		rightlatindices = WHERE(latList eq closest)	
        ; Store longitudes that have the closest latitude in 
        ; the rightlats variable
		rightlats=lonList[rightlatindices]
        ; We've transversed a certain distance down the list to the section			    ; containing the closest latitude and a variety of longitudes.
        ; Next we'll have to find the closest longitude, so store how 
        ; far down we went as firstlatindex		
		firstlatindex=rightlatindices[0]-1					
																
		; Now we need to search through all values with the 
        ; closest latitude for the one with the closest longitude
        ; get size of right lattitudes array. If only one value, 
        ; we need to do things differently
		ssize=size(rightlats) 
		if ssize[0] gt 1 then begin
            ; Find the closest longitude index
		    bin=VALUE_LOCATE(rightlats,lonArray[i]) 	
            ; If it is less than the first longitude, use the first longitude
			IF (bin eq -1) then begin					 	
				closest=rightlats[0]					 	
			ENDIF ELSE BEGIN
                ; If greater than the last longitude, use the last longitude
				IF (bin eq (N_ELEMENTS(rightlats)-1)) then begin 
					closest = rightlats[N_ELEMENTS(lonList)-1]
                ; Otherwise it's between two values, 
                ; so find which one it is closer to.				
                ENDIF ELSE BEGIN
					IF ( ABS(rightlats[bin] - lonArray[i]) GT ABS(rightlats[bin+1] - lonArray[i]) ) THEN BEGIN
              						closest = rightlats[bin+1]
					ENDIF ELSE BEGIN
						closest = rightlats[bin]
					ENDELSE
				ENDELSE
			ENDELSE	
			; Finally, find the index with the right latitudes where 
            ; the longitude equals the closest longitude
			rightlonindex=where(rightlats eq closest)
            ; Add however far down the list of right latitudes we had 
            ; to go to get to the right longitude to the distance we 
            ; had to go down the list to get to the first right latidude. 
            ; We finally have the location of the closest lat/long pair
			rightindex=rightlonindex+firstlatindex	

		endif else begin
            ; if only one element in rightlats
			rightindex=firstlatindex 
			;print,'only one correct latitude!'
		endelse
		;print,rightindex	;print how far we had to go, for debugging
        ;print, 'i'
        ;print, i
		;print,'CHECK VALUES BELOW CLOSE!'
		;print,latArray[i],',',lonArray[i] ;print the latitude/longitude we were trying to find the closest value to, for debugging
		closeststring=STRTRIM(STRING(latList[rightindex]),1)+','+STRTRIM(STRING(lonList[rightindex]),1)
        ; We print the closest lat/long. in the cmap, for debugging
		;print, closeststring 
		emissionangle=muList[rightindex] ;set angles
		incidenceangle=mu0List[rightindex]
        muArray[i] = emissionangle
        mu0Array[i] = incidenceangle
        ; Generate text to update display to user
		emissionangle='Emission angle:'+STRING(emissionangle) 
		incidenceangle='Incidence angle:'+STRING(incidenceangle)
        ;print, emissionangle
        ;print, incidenceangle
    endfor
end


