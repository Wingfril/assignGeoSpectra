pro getHeaderInfo, gfileType, guidePath, guideFiles, outputGuideFile, $
                   sfileType, spectraPath, spectraFiles, outputSpectraFile, $
                   outputPath

	;gfileType = 'guideImages'
	;guidePath = '/home/mziyan/TestData/17May20/guideimgPRISM/'
	;guideFiles = file_search(guidePath+'jup.00*.fits')
	
	;outputGuideFile = 'guideTime.reftable'	
	;outputPath = '/home/mziyan/TestData/17May20/'	
	openw,1,outputPath+outputGuideFile,/APPEND
	printf,1,format='(A,",",A)',gfileType, '		Time'


	guideFileSize = size (guideFiles, /DIMENSIONS)
	
	guideName = STRARR(guideFileSize[0])
	guideTime = STRARR(guideFileSize[0])

	for i = 0, guideFileSize[0]-1 do begin
		header = headfits(guideFiles[i])
		time = fxpar(header,'TIME_OBS')
		;name = gfileType + STRMID(guideFiles[i], STRLEN(guidePath))
        name = guideFiles[i]

		guideName[i] = name
		guideTime[i] = time
		printf, 1, format='(A,",",A)',name, time

	endfor
	close,1

	;sfileType = 'SpectraImages'
	;spectraPath = '/prvt/cirs12/spex/2017may20/spectra/'
	;spectraFiles = file_search(spectraPath + 'jup.00*.fits')
	;outputSpectraFile = 'spectraTime.reftable'	
	;outputPath = '/home/mziyan/TestData/17May20/'	

	openu,2,outputPath+outputSpectraFile,/APPEND
	printf,2,format='(A,",",A)',sfileType, '		Time'

	spectraFileSize = size (spectraFiles, /DIMENSIONS)
	
	spectraName = STRARR(spectraFileSize[0])
	spectraTime = STRARR(spectraFileSize[0])

	for i = 0, spectraFileSize[0]-1 do begin
		header = headfits(spectraFiles[i])
		time = fxpar(header,'TIME_OBS')
		;name = sfileType + STRMID(spectraFiles[i], STRLEN(spectraPath))
        name = spectraFiles[i]
    
		spectraName[i] = name
		spectraTime[i] = time
		printf, 2, format='(A,",",A)',name, time
	endfor	
	close,2
	;WRITE_CSV, outputPath+outputSpectraFile, spectraName, spectraTime

end
