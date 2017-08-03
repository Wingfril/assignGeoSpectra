pro spexDriver, guidePath, spectraPath, outputPath, calpath
    gfileType = 'guideImages'
	;guidePath = '/home/mziyan/TestData/17Mar29/guideimg/'
	tempGF = file_search(guidePath+'*')
    guideFiles = tempGF[where(strmatch(tempGF, '*reftable*')eq 0)]
    outputGuideFile = 'guideTime.reftable'	

	sfileType = 'SpectraImages'
	;spectraPath = '/home/mziyan/TestData/17Mar29/rawspectra/'
	spectraFiles = file_search(spectraPath + '*jup.00*.fits')
	outputSpectraFile = 'spectraTime.reftable'

	;outputPath = '/home/mziyan/TestData/17Mar29/'

    ; Get the time information from header files and put it into a cvs
    ; for both spectra and guide images	
    ;getHeaderInfo, gfileType, guidePath, guideFiles, outputGuideFile, $
    ;               sfileType, spectraPath, spectraFiles, outputSpectraFile, $
    ;               outputPath
    outputName = 'matchedGuidesAndSpectra.reftable'

    ; Based on the time, match guide images with spectra and output into 
    ; csv
    ;findMatch, outputPath+outputGuideFile, outputPath+outputSpectraFile, $
    ;           outputName, outputPath 

    ; Retrieve the matched guide image and spectra from csv from above
    specGuideInfo = read_csv(outputPath+outputName)
    spectra = specGuideInfo.field1
    guideImage = specGuideInfo.field3
    print, specGuideInfo.field1
    print, outputPath + outputName
    siz = size(spectra, /DIMENSIONS)
    flatpaths = file_search(calpath+'*flat*')
    wavecalPath = file_search(calpath+'*wavecal*')
    
    if N_ELEMENTS(flatpaths) eq 0 or N_ELEMENTS(wavecalPath) eq 0 then begin
    	print, 'calibration files not found in path' + flatpath
	goto, ENDEVENT
    endif
    print, spectra
    ; Loop through the amount of raw spectra files
    for i = 0, siz[0]-2, 2 do begin

        ; Making the array with an A and a B spectra
        tempArr = [specGuideInfo.field1[i], specGuideInfo.field1[i+1]]

        num1 = strmid(specGuideInfo.field1[i], strlen(specGuideInfo.field1[i])-10, 3)
        if fix(num1) lt 100 then begin
            num1 = strmid(num1, 1)        
        endif
        num2 = strmid(spectra[i+1], strlen(spectra[i+1])-10, 3)
        if fix(num2) lt 100 then begin
            num2 = strmid(num2, 1)        
        endif
        files = [num1, num2]
        print, files
        ; loaded image
        spextool_zmo_loadImage, flatPaths[0], wavecalPath[0], tempArr, 'A-B', files, /CLEAR, /BEEP
        
        ; Guide Image problems
        ; assigned geometry
        ;print, guideImage[i]
        zmo_inputGeometry, guideImage[i]

    endfor
    ENDEVENT: print, 'Exiting spexDriver...'
end

