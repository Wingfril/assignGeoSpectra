pro assignGeoCSV, num
    outputPath = '/home/mziyan/TestData/16may12/guideimg/'+num
    ending = '.a.reftable'
    img = readfits(outputPath, header)

    xspextool_zmo_procguide, img, header, outputPath+ending
end
