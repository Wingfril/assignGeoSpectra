
function stringToSec, timesStr, modding = modding

	; Example time: 06:15:35.030274
	timesStr = STRSPLIT(timesStr, ':', /EXTRACT)
	; Getting the 06
	hourStr = timesStr[0]
	; Getting the 15 ( we start from position 2 because
	; we have to account for the space after the hour
	minutesStr = timesStr[1]
	secondsStr = timesStr[2]
	if hourStr eq '12' then begin
		modding = 1
	endif

	; Converting the time strings into floats, 
	; then converting them into seconds
	if modding eq 1 and FLOAT(hourStr) < 4 then begin
		hour = FLOAT(hourStr) + 12
	endif else begin
		hour = FLOAT(hourStr)
	endelse
	totalSeconds = 0
	totalSeconds += hour * 3600 + FLOAT(minutesStr) * 60
	totalSeconds += FLOAT(secondsStr)
return, totalSeconds
end

; Attempts to find guide images and their respective spectra. 
; It does this by reading the CSV files created by the other program, 
; getHeaderTime. 
; We convert the time into seconds, and compare the time in guide images
; to time in spectra. 
; If the times do not match up at all, find the difference between the first
; spectra and the first guide image. Apply the difference to all of the spectra
; and attempt to match it up. 

pro findMatch 	
	
	; Hard coding in the path for now
	guideRefPath = '/home/mziyan/TestData/17May20/guideTime.reftable'
	specRefPath = '/home/mziyan/TestData/17May20/spectraTime.reftable'
	outputName = 'matchedGuidesAndSpectra.reftable'	
	outputPath = '/home/mziyan/TestData/17May20/'

	; Retrieving guide names and guide times from the path above
	guideInfo = read_csv(guideRefPath, HEADER = guideHeader, N_TABLE_HEADER = 2, TABLE_HEADER = guideTableHeader)
	guideNames = guideInfo.field1
	guideTimesStr = guideInfo.field2

	; Retrieving spectra names and spectra times from the path above
	spectraInfo = read_csv(specRefPath, HEADER = spectraHeader, N_TABLE_HEADER = 2, TABLE_HEADER = spectraTableHeader)
	spectraNames = spectraInfo.field1
	spectraTimesStr = spectraInfo.field2
	; Set up array; since there are more spectra than guide images, 
	; we take the dimension of the spectra. There will be multiple
	; guide images assigned to each spectra. 
	guideNumber = SIZE(guideNames,/DIMENSIONS)
	spectraNumber = SIZE(spectraNames,/DIMENSIONS)
	resultArray = STRARR(spectraNumber[0], 4)

	guideTimes = FLTARR(guideNumber[0])
	spectraTimes = FLTARR(spectraNumber[0])

	modding = 0
	guideCount = 0
	maxGuideCount = guideNumber[0]
	print, maxGuideCount
	for i = 0, spectraNumber[0] - 1 do begin
		spectraTimes[i] = stringToSec(spectraTimesStr[i], modding = modding)
	endfor

	; Reset modding to see if we pass 12 o clock in guide
		
	modding = 0
	for i = 0, guideNumber[0] - 1 do begin
		guideTimes[i] = stringToSec(guideTimesStr[i], modding = modding)
	endfor

	if ((guideTimes[0] lt spectraTimes[0]) and $
	   (guideTimes[guideNumber[0]-1] lt spectraTimes[0])) or $
	   ((guideTimes[0] gt spectraTimes[0]) and $
	   (guideTimes[0] gt spectraTimes[spectraNumber[0]-1])) then begin
		; Need to figure something out if times mismatch mmmm lovely. 
	endif else begin

		; We keep the a & b files together, so we increment by 2 every time. 
		for i = 0, spectraNumber[0] - 1, 2 do begin
			; Put in the name and time first
			resultArray[i,0] = spectraNames[i]
			resultArray[i+1,0] = spectraNames[i+1]
			resultArray[i,1] = spectraTimes[i]
			resultArray[i+1,1] = spectraTimes[i+1]

			; If the b times are greater than guide times, we need to 
			; increment
			if spectraTimes[i+1] gt guideTimes[guideCount] then begin
				guideCount += 1
			endif
			
			; Checking if there is a sky image associated with the 
			; guide image by looking at difference in times
			if guideCount+2 lt maxGuideCount then begin
				if guideTimes[guideCount+1] - guideTimes[guideCount] lt 20 then begin
					guideCount += 1
				endif
			endif

			; There will be images taken after the last guide image;
			; put it with the last guide image
			if maxGuideCount lt guideCount+2 then begin
				guideCount = maxGuideCount-1
			endif
			resultArray[i,2] = guideNames[guideCount]
			resultArray[i+1,2] = guideNames[guideCount]
			resultArray[i,3] = guideTimes[guideCount]
			resultArray[i+1,3] = guideTimes[guideCount]
			
		endfor
	endelse	

	; Output and write to a CVS file
	openw, 1, outputPath+outputName,/APPEND
	printf, 1, format='(A,",",A, ",", A, ",", A)', 'spectra','		Time',$
							'guides','		Time' 
	for i = 0, spectraNumber[0] - 1, 1 do begin
		printf, 1, format='(A,",",A, ",", A, ",", A)', $
				resultArray[i, 0], resultArray[i, 1], $
				resultArray[i, 2], resultArray[i, 3]
	endfor
	close, 1
end
