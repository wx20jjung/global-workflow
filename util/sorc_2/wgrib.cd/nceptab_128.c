#include "cnames.h"

const struct ParmTable parm_table_nceptab_128[256] = {
      /* 0 */ {"var0", "undefined"},
      /* 1 */ {"PRES", "Pressure [Pa]"},
      /* 2 */ {"PRMSL", "Pressure reduced to MSL [Pa]"},
      /* 3 */ {"PTEND", "Pressure tendency [Pa/s]"},
      /* 4 */ {"PVORT", "Pot. vorticity [km^2/kg/s]"},
      /* 5 */ {"ICAHT", "ICAO Standard Atmosphere Reference Height [M]"},
      /* 6 */ {"GP", "Geopotential [m^2/s^2]"},
      /* 7 */ {"HGT", "Geopotential height [gpm]"},
      /* 8 */ {"DIST", "Geometric height [m]"},
      /* 9 */ {"HSTDV", "Std dev of height [m]"},
      /* 10 */ {"TOZNE", "Total ozone [Dobson]"},
      /* 11 */ {"TMP", "Temp. [K]"},
      /* 12 */ {"VTMP", "Virtual temp. [K]"},
      /* 13 */ {"POT", "Potential temp. [K]"},
      /* 14 */ {"EPOT", "Pseudo-adiabatic pot. temp. [K]"},
      /* 15 */ {"TMAX", "Max. temp. [K]"},
      /* 16 */ {"TMIN", "Min. temp. [K]"},
      /* 17 */ {"DPT", "Dew point temp. [K]"},
      /* 18 */ {"DEPR", "Dew point depression [K]"},
      /* 19 */ {"LAPR", "Lapse rate [K/m]"},
      /* 20 */ {"VIS", "Visibility [m]"},
      /* 21 */ {"RDSP1", "Radar spectra (1) [non-dim]"},
      /* 22 */ {"RDSP2", "Radar spectra (2) [non-dim]"},
      /* 23 */ {"RDSP3", "Radar spectra (3) [non-dim]"},
      /* 24 */ {"PLI", "Parcel lifted index (to 500 hPa) [K]"},
      /* 25 */ {"TMPA", "Temp. anomaly [K]"},
      /* 26 */ {"PRESA", "Pressure anomaly [Pa]"},
      /* 27 */ {"GPA", "Geopotential height anomaly [gpm]"},
      /* 28 */ {"WVSP1", "Wave spectra (1) [non-dim]"},
      /* 29 */ {"WVSP2", "Wave spectra (2) [non-dim]"},
      /* 30 */ {"WVSP3", "Wave spectra (3) [non-dim]"},
      /* 31 */ {"WDIR", "Wind direction [deg]"},
      /* 32 */ {"WIND", "Wind speed [m/s]"},
      /* 33 */ {"UGRD", "u wind [m/s]"},
      /* 34 */ {"VGRD", "v wind [m/s]"},
      /* 35 */ {"STRM", "Stream function [m^2/s]"},
      /* 36 */ {"VPOT", "Velocity potential [m^2/s]"},
      /* 37 */ {"MNTSF", "Montgomery stream function [m^2/s^2]"},
      /* 38 */ {"SGCVV", "Sigma coord. vertical velocity [/s]"},
      /* 39 */ {"VVEL", "Pressure vertical velocity [Pa/s]"},
      /* 40 */ {"DZDT", "Geometric vertical velocity [m/s]"},
      /* 41 */ {"ABSV", "Absolute vorticity [/s]"},
      /* 42 */ {"ABSD", "Absolute divergence [/s]"},
      /* 43 */ {"RELV", "Relative vorticity [/s]"},
      /* 44 */ {"RELD", "Relative divergence [/s]"},
      /* 45 */ {"VUCSH", "Vertical u shear [/s]"},
      /* 46 */ {"VVCSH", "Vertical v shear [/s]"},
      /* 47 */ {"DIRC", "Direction of current [deg]"},
      /* 48 */ {"SPC", "Speed of current [m/s]"},
      /* 49 */ {"UOGRD", "u of current [m/s]"},
      /* 50 */ {"VOGRD", "v of current [m/s]"},
      /* 51 */ {"SPFH", "Specific humidity [kg/kg]"},
      /* 52 */ {"RH", "Relative humidity [%]"},
      /* 53 */ {"MIXR", "Humidity mixing ratio [kg/kg]"},
      /* 54 */ {"PWAT", "Precipitable water [kg/m^2]"},
      /* 55 */ {"VAPP", "Vapor pressure [Pa]"},
      /* 56 */ {"SATD", "Saturation deficit [Pa]"},
      /* 57 */ {"EVP", "Evaporation [kg/m^2]"},
      /* 58 */ {"CICE", "Cloud Ice [kg/m^2]"},
      /* 59 */ {"PRATE", "Precipitation rate [kg/m^2/s]"},
      /* 60 */ {"TSTM", "Thunderstorm probability [%]"},
      /* 61 */ {"APCP", "Total precipitation [kg/m^2]"},
      /* 62 */ {"NCPCP", "Large scale precipitation [kg/m^2]"},
      /* 63 */ {"ACPCP", "Convective precipitation [kg/m^2]"},
      /* 64 */ {"SRWEQ", "Snowfall rate water equiv. [kg/m^2/s]"},
      /* 65 */ {"WEASD", "Accum. snow [kg/m^2]"},
      /* 66 */ {"SNOD", "Snow depth [m]"},
      /* 67 */ {"MIXHT", "Mixed layer depth [m]"},
      /* 68 */ {"TTHDP", "Transient thermocline depth [m]"},
      /* 69 */ {"MTHD", "Main thermocline depth [m]"},
      /* 70 */ {"MTHA", "Main thermocline anomaly [m]"},
      /* 71 */ {"TCDC", "Total cloud cover [%]"},
      /* 72 */ {"CDCON", "Convective cloud cover [%]"},
      /* 73 */ {"LCDC", "Low level cloud cover [%]"},
      /* 74 */ {"MCDC", "Mid level cloud cover [%]"},
      /* 75 */ {"HCDC", "High level cloud cover [%]"},
      /* 76 */ {"CWAT", "Cloud water [kg/m^2]"},
      /* 77 */ {"BLI", "Best lifted index (to 500 hPa) [K]"},
      /* 78 */ {"SNOC", "Convective snow [kg/m^2]"},
      /* 79 */ {"SNOL", "Large scale snow [kg/m^2]"},
      /* 80 */ {"WTMP", "Water temp. [K]"},
      /* 81 */ {"LAND", "Land cover (land=1;sea=0) [fraction]"},
      /* 82 */ {"DSLM", "Deviation of sea level from mean [m]"},
      /* 83 */ {"SFCR", "Surface roughness [m]"},
      /* 84 */ {"ALBDO", "Albedo [%]"},
      /* 85 */ {"TSOIL", "Soil temp. [K]"},
      /* 86 */ {"SOILM", "Soil moisture content [kg/m^2]"},
      /* 87 */ {"VEG", "Vegetation [%]"},
      /* 88 */ {"SALTY", "Salinity [kg/kg]"},
      /* 89 */ {"DEN", "Density [kg/m^3]"},
      /* 90 */ {"WATR", "Water runoff [kg/m^2]"},
      /* 91 */ {"ICEC", "Ice concentration (ice=1;no ice=0) [fraction]"},
      /* 92 */ {"ICETK", "Ice thickness [m]"},
      /* 93 */ {"DICED", "Direction of ice drift [deg]"},
      /* 94 */ {"SICED", "Speed of ice drift [m/s]"},
      /* 95 */ {"UICE", "u of ice drift [m/s]"},
      /* 96 */ {"VICE", "v of ice drift [m/s]"},
      /* 97 */ {"ICEG", "Ice growth rate [m/s]"},
      /* 98 */ {"ICED", "Ice divergence [/s]"},
      /* 99 */ {"SNOM", "Snow melt [kg/m^2]"},
      /* 100 */ {"HTSGW", "Sig height of wind waves and swell [m]"},
      /* 101 */ {"WVDIR", "Direction of wind waves [deg]"},
      /* 102 */ {"WVHGT", "Sig height of wind waves [m]"},
      /* 103 */ {"WVPER", "Mean period of wind waves [s]"},
      /* 104 */ {"SWDIR", "Direction of swell waves [deg]"},
      /* 105 */ {"SWELL", "Sig height of swell waves [m]"},
      /* 106 */ {"SWPER", "Mean period of swell waves [s]"},
      /* 107 */ {"DIRPW", "Primary wave direction [deg]"},
      /* 108 */ {"PERPW", "Primary wave mean period [s]"},
      /* 109 */ {"DIRSW", "Secondary wave direction [deg]"},
      /* 110 */ {"PERSW", "Secondary wave mean period [s]"},
      /* 111 */ {"NSWRS", "Net short wave (surface) [W/m^2]"},
      /* 112 */ {"NLWRS", "Net long wave (surface) [W/m^2]"},
      /* 113 */ {"NSWRT", "Net short wave (top) [W/m^2]"},
      /* 114 */ {"NLWRT", "Net long wave (top) [W/m^2]"},
      /* 115 */ {"LWAVR", "Long wave [W/m^2]"},
      /* 116 */ {"SWAVR", "Short wave [W/m^2]"},
      /* 117 */ {"GRAD", "Global radiation [W/m^2]"},
      /* 118 */ {"BRTMP", "Brightness temperature [K]"},
      /* 119 */ {"LWRAD", "Radiance with respect to wave no. [W/m/sr]"},
      /* 120 */ {"SWRAD", "Radiance with respect ot wave len. [W/m^3/sr]"},
      /* 121 */ {"LHTFL", "Latent heat flux [W/m^2]"},
      /* 122 */ {"SHTFL", "Sensible heat flux [W/m^2]"},
      /* 123 */ {"BLYDP", "Boundary layer dissipation [W/m^2]"},
      /* 124 */ {"UFLX", "Zonal momentum flux [N/m^2]"},
      /* 125 */ {"VFLX", "Meridional momentum flux [N/m^2]"},
      /* 126 */ {"WMIXE", "Wind mixing energy [J]"},
      /* 127 */ {"IMGD", "Image data []"},
      /* 128 */ {"AVDEPTH", "Ocean depth - mean [m]"},
      /* 129 */ {"DEPTH", "Ocean depth - instantaneous [m]"},
      /* 130 */ {"ELEV", "Ocean surface elevation relative to geoid [m]"},
      /* 131 */ {"MXEL24", "Max ocean surface elevation in last 24 hours [m]"},
      /* 132 */ {"MNEL24", "Min ocean surface elevation in last 24 hours [m]"},
      /* 133 */ {"var133", "undefined"},
      /* 134 */ {"var134", "undefined"},
      /* 135 */ {"O2", "Oxygen (O2 (aq)) []"},
      /* 136 */ {"PO4", "PO4 [Mol/kg]"},
      /* 137 */ {"NO3", "NO3 [Mol/kg]"},
      /* 138 */ {"SiO4", "SiO4 [Mol/kg]"},
      /* 139 */ {"CO2aq", "CO2 (aq) [Mol/kg]"},
      /* 140 */ {"HCO3", "HCO3 - [Mol/kg]"},
      /* 141 */ {"CO3", "CO3 -- [Mol/kg]"},
      /* 142 */ {"TCO2", "TCO2 [Mol/kg]"},
      /* 143 */ {"TALK", "TALK [Mol/kg]"},
      /* 144 */ {"var144", "undefined"},
      /* 145 */ {"var145", "undefined"},
      /* 146 */ {"S11", "S11 - 1,1 component of ice stress tensor []"},
      /* 147 */ {"S12", "S12 - 1,2 component of ice stress tensor []"},
      /* 148 */ {"S22", "S22 - 2,2 component of ice stress tensor []"},
      /* 149 */ {"INV1", "T1 - First invariant of stress tensor []"},
      /* 150 */ {"INV2", "T2 - Second invariant of stress tensor []"},
      /* 151 */ {"var151", "undefined"},
      /* 152 */ {"var152", "undefined"},
      /* 153 */ {"var153", "undefined"},
      /* 154 */ {"var154", "undefined"},
      /* 155 */ {"WVRGH", "Wave Roughness[ ]"},
      /* 156 */ {"WVSTRS", "Wave Stresses []"},
      /* 157 */ {"WHITE", "Whitecap coverage []"},
      /* 158 */ {"SWDIRWID", "Swell direction width []"},
      /* 159 */ {"SWFREWID", "Swell frequency width []"},
      /* 160 */ {"WVAGE", "Wave age []"},
      /* 161 */ {"PWVAGE", "Physical Wave age []"},
      /* 162 */ {"var162", "undefined"},
      /* 163 */ {"var163", "undefined"},
      /* 164 */ {"var164", "undefined"},
      /* 165 */ {"LTURB", "Master length scale (turbulence) [m]"},
      /* 166 */ {"var166", "undefined"},
      /* 167 */ {"var167", "undefined"},
      /* 168 */ {"var168", "undefined"},
      /* 169 */ {"var169", "undefined"},
      /* 170 */ {"AIHFLX", "Net Air-Ice heat flux [W/m^2]"},
      /* 171 */ {"AOHFLX", "Net Air-Ocean heat flux [W/m^2]"},
      /* 172 */ {"IOHFLX", "Net Ice-Ocean heat flux [W/m^2]"},
      /* 173 */ {"IOSFLX", "Net Ice-Ocean salt flux kg/s]"},
      /* 174 */ {"var174", "undefined"},
      /* 175 */ {"OMLT", "Ocean Mixed Layer Temperature [K]"},
      /* 176 */ {"OMLS", "Ocean Mixed Layer Salinity [kg/kg]"},
      /* 177 */ {"OMLPOTDEN", "Ocean Mixed Layer Potential Density (Referenced to 2000m) [kg/m^3]"},
      /* 178 */ {"OMLU", "U Velocity in mixed layer [m/s]"},
      /* 179 */ {"OMLV", "V Velocity in mixed layer [m/s]"},
      /* 180 */ {"ASHFL", "Assimilative Heat Flux [W/m^2]"},
      /* 181 */ {"ASSFL", "Assimilative Salt Flux [mm/day]"},
      /* 182 */ {"BOTLD", "Bottom Layer Depth [m]"},
      /* 183 */ {"UBARO", "Barotropic U Velocity [m/s]"},
      /* 184 */ {"VBARO", "Barotropic V Velocity [m/s]"},
      /* 185 */ {"INTFD", "Interface Depth [m]"},
      /* 186 */ {"WTMPC", "Temperature [C]"},
      /* 187 */ {"SALIN", "Salinity [psu]"},
      /* 188 */ {"EMNP", "Evaporation - Precipitation [cm/day]"},
      /* 189 */ {"var189", "undefined"},
      /* 190 */ {"KENG", "Kinetic Energy [J/kg]"},
      /* 191 */ {"var191", "undefined"},
      /* 192 */ {"LAYTH", "Layer Thickness[m]"},
      /* 193 */ {"SSTT", "Surface Temperature Trend [K/day]"},
      /* 194 */ {"SSST", "Surface Salinity Trend [psu/day]"},
      /* 195 */ {"var195", "undefined"},
      /* 196 */ {"var196", "undefined"},
      /* 197 */ {"var197", "undefined"},
      /* 198 */ {"var198", "undefined"},
      /* 199 */ {"var199", "undefined"},
      /* 200 */ {"var200", "undefined"},
      /* 201 */ {"var201", "undefined"},
      /* 202 */ {"var202", "undefined"},
      /* 203 */ {"var203", "undefined"},
      /* 204 */ {"var204", "undefined"},
      /* 205 */ {"var205", "undefined"},
      /* 206 */ {"var206", "undefined"},
      /* 207 */ {"var207", "undefined"},
      /* 208 */ {"var208", "undefined"},
      /* 209 */ {"var209", "undefined"},
      /* 210 */ {"var210", "undefined"},
      /* 211 */ {"var211", "undefined"},
      /* 212 */ {"var212", "undefined"},
      /* 213 */ {"var213", "undefined"},
      /* 214 */ {"var214", "undefined"},
      /* 215 */ {"var215", "undefined"},
      /* 216 */ {"var216", "undefined"},
      /* 217 */ {"var217", "undefined"},
      /* 218 */ {"var218", "undefined"},
      /* 219 */ {"var219", "undefined"},
      /* 220 */ {"var220", "undefined"},
      /* 221 */ {"var221", "undefined"},
      /* 222 */ {"var222", "undefined"},
      /* 223 */ {"var223", "undefined"},
      /* 224 */ {"var224", "undefined"},
      /* 225 */ {"var225", "undefined"},
      /* 226 */ {"var226", "undefined"},
      /* 227 */ {"var227", "undefined"},
      /* 228 */ {"var228", "undefined"},
      /* 229 */ {"var229", "undefined"},
      /* 230 */ {"var230", "undefined"},
      /* 231 */ {"var231", "undefined"},
      /* 232 */ {"var232", "undefined"},
      /* 233 */ {"var233", "undefined"},
      /* 234 */ {"var234", "undefined"},
      /* 235 */ {"var235", "undefined"},
      /* 236 */ {"var236", "undefined"},
      /* 237 */ {"var237", "undefined"},
      /* 238 */ {"var238", "undefined"},
      /* 239 */ {"var239", "undefined"},
      /* 240 */ {"var240", "undefined"},
      /* 241 */ {"var241", "undefined"},
      /* 242 */ {"var242", "undefined"},
      /* 243 */ {"var243", "undefined"},
      /* 244 */ {"var244", "undefined"},
      /* 245 */ {"var245", "undefined"},
      /* 246 */ {"var246", "undefined"},
      /* 247 */ {"var247", "undefined"},
      /* 248 */ {"var248", "undefined"},
      /* 249 */ {"var249", "undefined"},
      /* 250 */ {"var250", "undefined"},
      /* 251 */ {"var251", "undefined"},
      /* 252 */ {"var252", "undefined"},
      /* 253 */ {"var253", "undefined"},
      /* 254 */ {"RERRVAR", "Relative Error Variance [pure number]"},
      /* 255 */ {"var255", "undefined"},
};