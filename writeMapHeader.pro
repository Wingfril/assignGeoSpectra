pro writeMapHeader, outputpath, geometryPath, spectraPath, guideList, $
                    guideListCounter 
    finalHeader = headfits(outputPath)
    print, outputPath
    print, finalHeader
    fxaddpar,finalHeader,'GEOMPATH', geometryPath, 'Path to cvs of geometry'
    fxaddpar,finalHeader,'SPECPATH', spectraPath, 'Path to first spectra used'
    str = '------------------------------------------------------------------------------------------'
    fxaddpar,finalHeader,'-----', str, 'Last part messes up without this'
    for i = 0, guideListCounter do begin
        print, 'creating guide header; tempstr, i'
        tempstr = 'GUIDE' + STRTRIM(i, 1)
        print, tempstr
        print, i
        fxaddpar,finalHeader,tempstr, guideList[i], 'list of guides used'
    endfor
    print, finalHeader
    modfits, outputPath, 0, finalHeader
end
