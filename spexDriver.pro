pro spexDriver, guidePath, spectraPath, outputPath
    gfileType = 'guideImages'
	;guidePath = '/home/mziyan/TestData/17Mar29/guideimg/'
	guideFiles = file_search(guidePath+'*.gz')
    outputGuideFile = 'guideTime.reftable'	

	sfileType = 'SpectraImages'
	;spectraPath = '/home/mziyan/TestData/17Mar29/rawspectra/'
	spectraFiles = file_search(spectraPath + '*jup.00*.fits')
	outputSpectraFile = 'spectraTime.reftable'

	;outputPath = '/home/mziyan/TestData/17Mar29/'

    ; Get the time information from header files and put it into a cvs
    ; for both spectra and guide images	
    getHeaderInfo, gfileType, guidePath, guideFiles, outputGuideFile, $
                   sfileType, spectraPath, spectraFiles, outputSpectraFile, $
                   outputPath
    outputName = 'matchedGuidesAndSpectra.reftable'

    ; Based on the time, match guide images with spectra and output into 
    ; csv
    findMatch, outputPath+outputGuideFile, outputPath+outputSpectraFile, $
               outputName, outputPath 

    ; Retrieve the matched guide image and spectra from csv from above
    specGuideInfo = read_csv(outputPath+outputName, HEADER = specGuideHeader, $ 
                   N_TABLE_HEADER = 4, TABLE_HEADER = specGuideTableHeader)
    spectra = specGuideInfo.field1
    guideImage = specGuideInfo.field3
   
    siz = size(spectra, /DIMENSIONS)
    flatpath = '/home/mziyan/TestData/17Mar29/cal/flat448-452.fits'
    wavecalPath = '/home/mziyan/TestData/17Mar29/cal/wavecal453-453.fits'
    ; Loop through the amount of raw spectra files
    for i = 0, siz[0]-2, 2 do begin

        ; Making the array with an A and a B spectra
        tempArr = [spectra[i], spectra[i+1]]

        num1 = strmid(spectra[i], strlen(spectra[i])-10, 3)
        if fix(num1) lt 100 then begin
            num1 = strmid(num1, 1)        
        endif
        num2 = strmid(spectra[i+1], strlen(spectra[i+1])-10, 3)
        if fix(num2) lt 100 then begin
            num2 = strmid(num2, 1)        
        endif
        files = [num1, num2]
        
        ; loaded image
        spextool_zmo_loadImage, flatPath, wavecalPath, tempArr, 'A-B', files, /CLEAR, /BEEP
        
        ; Guide Image problems
        ; assigned geometry
        ;print, guideImage[i]
        zmo_inputGeometry, guideImage[i]

    endfor
    
end
