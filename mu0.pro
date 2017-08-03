pro mu0, lat, lon
lat *= !CONST.PI/ 180
print, 'lat'
print, lat
lcm = 228.21311
lon = (lon  + lcm - 360)* !CONST.PI/ 180
print, 'lon'
print, lon

o = 1.0693700

ras = 0.384740

rae = 0.220243

ds = -0.022288553


    czas = cos(atan(o*o*tan(lat)))*cos(ds)*cos(lon-(ras-rae)) + $
                  sin(atan(o*o*tan(lat)))*sin(ds)
print, 'mu0'
print, czas
end
