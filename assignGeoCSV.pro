pro assignGeoCSV, outputPath
    ending = '.a.reftable'
    img = readfits(outputPath, header)

    xspextool_zmo_procguide, img, header, outputPath+ending
end
