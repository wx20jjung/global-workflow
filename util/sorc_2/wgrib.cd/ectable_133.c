#include "cnames.h"

const struct ParmTable parm_table_ecmwf_133[256] = {
      /* 0 */ {"var0", "undefined"},
      /* 1 */ {"2TPLM10", "2m temperature probability less than -10 C [%]"},
      /* 2 */ {"2TPLM5", "2m temperature probability less than -5 C [%]"},
      /* 3 */ {"2TPL0", "2m temperature probability less than 0 C [%]"},
      /* 4 */ {"2TPL5", "2m temperature probability less than 5 C [%]"},
      /* 5 */ {"2TPL10", "2m temperature probability less than 10 C [%]"},
      /* 6 */ {"2TPG25", "2m temperature probability greater than 25 C [%]"},
      /* 7 */ {"2TPG30", "2m temperature probability greater than 30 C [%]"},
      /* 8 */ {"2TPG35", "2m temperature probability greater than 35 C [%]"},
      /* 9 */ {"2TPG40", "2m temperature probability greater than 40 C [%]"},
      /* 10 */ {"2TPG45", "2m temperature probability greater than 45 C [%]"},
      /* 11 */ {"MN2TPLM10", "Minimum 2 metre temperature probability less than -10 C [%]"},
      /* 12 */ {"MN2TPLM5", "Minimum 2 metre temperature probability less than -5 C [%]"},
      /* 13 */ {"MN2TPL0", "Minimum 2 metre temperature probability less than 0 C [%]"},
      /* 14 */ {"MN2TPL5", "Minimum 2 metre temperature probability less than 5 C [%]"},
      /* 15 */ {"MN2TPL10", "Minimum 2 metre temperature probability less than 10 C [%]"},
      /* 16 */ {"MX2TPG25", "Maximum 2 metre temperature probability greater than 25 C [%]"},
      /* 17 */ {"MX2TPG30", "Maximum 2 metre temperature probability greater than 30 C [%]"},
      /* 18 */ {"MX2TPG35", "Maximum 2 metre temperature probability greater than 35 C [%]"},
      /* 19 */ {"MX2TPG40", "Maximum 2 metre temperature probability greater than 40 C [%]"},
      /* 20 */ {"MX2TPG45", "Maximum 2 metre temperature probability greater than 45 C [%]"},
      /* 21 */ {"10SPG10", "10 metre wind speed probability of at least 10 m/s [%]"},
      /* 22 */ {"10SPG15", "10 metre wind speed probability of at least 15 m/s [%]"},
      /* 23 */ {"10SPG20", "10 metre wind speed probability of at least 20 m/s [%]"},
      /* 24 */ {"10SPG35", "10 metre wind speed probability of at least 35 m/s [%]"},
      /* 25 */ {"10SPG50", "10 metre wind speed probability of at least 50 m/s [%]"},
      /* 26 */ {"10GPG20", "10 metre wind gust probability of at least 20 m/s [%]"},
      /* 27 */ {"10GPG35", "10 metre wind gust probability of at least 35 m/s [%]"},
      /* 28 */ {"10GPG50", "10 metre wind gust probability of at least 50 m/s [%]"},
      /* 29 */ {"10GPG75", "10 metre wind gust probability of at least 75 m/s [%]"},
      /* 30 */ {"10GPG100", "10 metre wind gust probability of at least 100 m/s [%]"},
      /* 31 */ {"TPPG1", "Total precipitation probability of at least 1 mm [%]"},
      /* 32 */ {"TPPG5", "Total precipitation probability of at least 5 mm [%]"},
      /* 33 */ {"TPPG10", "Total precipitation probability of at least 10 mm [%]"},
      /* 34 */ {"TPPG20", "Total precipitation probability of at least 20 mm [%]"},
      /* 35 */ {"TPPG40", "Total precipitation probability of at least 40 mm [%]"},
      /* 36 */ {"TPPG60", "Total precipitation probability of at least 60 mm [%]"},
      /* 37 */ {"TPPG80", "Total precipitation probability of at least 80 mm [%]"},
      /* 38 */ {"TPPG100", "Total precipitation probability of at least 100 mm [%]"},
      /* 39 */ {"TPPG150", "Total precipitation probability of at least 150 mm [%]"},
      /* 40 */ {"TPPG200", "Total precipitation probability of at least 200 mm [%]"},
      /* 41 */ {"TPPG300", "Total precipitation probability of at least 300 mm [%]"},
      /* 42 */ {"SFPG1", "Snowfall probability of at least 1 mm [%]"},
      /* 43 */ {"SFPG5", "Snowfall probability of at least 5 mm [%]"},
      /* 44 */ {"SFPG10", "Snowfall probability of at least 10 mm [%]"},
      /* 45 */ {"SFPG20", "Snowfall probability of at least 20 mm [%]"},
      /* 46 */ {"SFPG40", "Snowfall probability of at least 40 mm [%]"},
      /* 47 */ {"SFPG60", "Snowfall probability of at least 60 mm [%]"},
      /* 48 */ {"SFPG80", "Snowfall probability of at least 80 mm [%]"},
      /* 49 */ {"SFPG100", "Snowfall probability of at least 100 mm [%]"},
      /* 50 */ {"SFPG150", "Snowfall probability of at least 150 mm [%]"},
      /* 51 */ {"SFPG200", "Snowfall probability of at least 200 mm [%]"},
      /* 52 */ {"SFPG300", "Snowfall probability of at least 300 mm [%]"},
      /* 53 */ {"TCCPG10", "Total Cloud Cover probability greater than 10% [%]"},
      /* 54 */ {"TCCPG20", "Total Cloud Cover probability greater than 20% [%]"},
      /* 55 */ {"TCCPG30", "Total Cloud Cover probability greater than 30% [%]"},
      /* 56 */ {"TCCPG40", "Total Cloud Cover probability greater than 40% [%]"},
      /* 57 */ {"TCCPG50", "Total Cloud Cover probability greater than 50% [%]"},
      /* 58 */ {"TCCPG60", "Total Cloud Cover probability greater than 60% [%]"},
      /* 59 */ {"TCCPG70", "Total Cloud Cover probability greater than 70% [%]"},
      /* 60 */ {"TCCPG80", "Total Cloud Cover probability greater than 80% [%]"},
      /* 61 */ {"TCCPG90", "Total Cloud Cover probability greater than 90% [%]"},
      /* 62 */ {"TCCPG99", "Total Cloud Cover probability greater than 99% [%]"},
      /* 63 */ {"HCCPG10", "High Cloud Cover probability greater than 10% [%]"},
      /* 64 */ {"HCCPG20", "High Cloud Cover probability greater than 20% [%]"},
      /* 65 */ {"HCCPG30", "High Cloud Cover probability greater than 30% [%]"},
      /* 66 */ {"HCCPG40", "High Cloud Cover probability greater than 40% [%]"},
      /* 67 */ {"HCCPG50", "High Cloud Cover probability greater than 50% [%]"},
      /* 68 */ {"HCCPG60", "High Cloud Cover probability greater than 60% [%]"},
      /* 69 */ {"HCCPG70", "High Cloud Cover probability greater than 70% [%]"},
      /* 70 */ {"HCCPG80", "High Cloud Cover probability greater than 80% [%]"},
      /* 71 */ {"HCCPG90", "High Cloud Cover probability greater than 90% [%]"},
      /* 72 */ {"HCCPG99", "High Cloud Cover probability greater than 99% [%]"},
      /* 73 */ {"MCCPG10", "Medium Cloud Cover probability greater than 10% [%]"},
      /* 74 */ {"MCCPG20", "Medium Cloud Cover probability greater than 20% [%]"},
      /* 75 */ {"MCCPG30", "Medium Cloud Cover probability greater than 30% [%]"},
      /* 76 */ {"MCCPG40", "Medium Cloud Cover probability greater than 40% [%]"},
      /* 77 */ {"MCCPG50", "Medium Cloud Cover probability greater than 50% [%]"},
      /* 78 */ {"MCCPG60", "Medium Cloud Cover probability greater than 60% [%]"},
      /* 79 */ {"MCCPG70", "Medium Cloud Cover probability greater than 70% [%]"},
      /* 80 */ {"MCCPG80", "Medium Cloud Cover probability greater than 80% [%]"},
      /* 81 */ {"MCCPG90", "Medium Cloud Cover probability greater than 90% [%]"},
      /* 82 */ {"MCCPG99", "Medium Cloud Cover probability greater than 99% [%]"},
      /* 83 */ {"LCCPG10", "Low Cloud Cover probability greater than 10% [%]"},
      /* 84 */ {"LCCPG20", "Low Cloud Cover probability greater than 20% [%]"},
      /* 85 */ {"LCCPG30", "Low Cloud Cover probability greater than 30% [%]"},
      /* 86 */ {"LCCPG40", "Low Cloud Cover probability greater than 40% [%]"},
      /* 87 */ {"LCCPG50", "Low Cloud Cover probability greater than 50% [%]"},
      /* 88 */ {"LCCPG60", "Low Cloud Cover probability greater than 60% [%]"},
      /* 89 */ {"LCCPG70", "Low Cloud Cover probability greater than 70% [%]"},
      /* 90 */ {"LCCPG80", "Low Cloud Cover probability greater than 80% [%]"},
      /* 91 */ {"LCCPG90", "Low Cloud Cover probability greater than 90% [%]"},
      /* 92 */ {"LCCPG99", "Low Cloud Cover probability greater than 99% [%]"},
      /* 93 */ {"var93", "undefined"},
      /* 94 */ {"var94", "undefined"},
      /* 95 */ {"var95", "undefined"},
      /* 96 */ {"var96", "undefined"},
      /* 97 */ {"var97", "undefined"},
      /* 98 */ {"var98", "undefined"},
      /* 99 */ {"var99", "undefined"},
      /* 100 */ {"var100", "undefined"},
      /* 101 */ {"var101", "undefined"},
      /* 102 */ {"var102", "undefined"},
      /* 103 */ {"var103", "undefined"},
      /* 104 */ {"var104", "undefined"},
      /* 105 */ {"var105", "undefined"},
      /* 106 */ {"var106", "undefined"},
      /* 107 */ {"var107", "undefined"},
      /* 108 */ {"var108", "undefined"},
      /* 109 */ {"var109", "undefined"},
      /* 110 */ {"var110", "undefined"},
      /* 111 */ {"var111", "undefined"},
      /* 112 */ {"var112", "undefined"},
      /* 113 */ {"var113", "undefined"},
      /* 114 */ {"var114", "undefined"},
      /* 115 */ {"var115", "undefined"},
      /* 116 */ {"var116", "undefined"},
      /* 117 */ {"var117", "undefined"},
      /* 118 */ {"var118", "undefined"},
      /* 119 */ {"var119", "undefined"},
      /* 120 */ {"var120", "undefined"},
      /* 121 */ {"var121", "undefined"},
      /* 122 */ {"var122", "undefined"},
      /* 123 */ {"var123", "undefined"},
      /* 124 */ {"var124", "undefined"},
      /* 125 */ {"var125", "undefined"},
      /* 126 */ {"var126", "undefined"},
      /* 127 */ {"var127", "undefined"},
      /* 128 */ {"var128", "undefined"},
      /* 129 */ {"var129", "undefined"},
      /* 130 */ {"var130", "undefined"},
      /* 131 */ {"var131", "undefined"},
      /* 132 */ {"var132", "undefined"},
      /* 133 */ {"var133", "undefined"},
      /* 134 */ {"var134", "undefined"},
      /* 135 */ {"var135", "undefined"},
      /* 136 */ {"var136", "undefined"},
      /* 137 */ {"var137", "undefined"},
      /* 138 */ {"var138", "undefined"},
      /* 139 */ {"var139", "undefined"},
      /* 140 */ {"var140", "undefined"},
      /* 141 */ {"var141", "undefined"},
      /* 142 */ {"var142", "undefined"},
      /* 143 */ {"var143", "undefined"},
      /* 144 */ {"var144", "undefined"},
      /* 145 */ {"var145", "undefined"},
      /* 146 */ {"var146", "undefined"},
      /* 147 */ {"var147", "undefined"},
      /* 148 */ {"var148", "undefined"},
      /* 149 */ {"var149", "undefined"},
      /* 150 */ {"var150", "undefined"},
      /* 151 */ {"var151", "undefined"},
      /* 152 */ {"var152", "undefined"},
      /* 153 */ {"var153", "undefined"},
      /* 154 */ {"var154", "undefined"},
      /* 155 */ {"var155", "undefined"},
      /* 156 */ {"var156", "undefined"},
      /* 157 */ {"var157", "undefined"},
      /* 158 */ {"var158", "undefined"},
      /* 159 */ {"var159", "undefined"},
      /* 160 */ {"var160", "undefined"},
      /* 161 */ {"var161", "undefined"},
      /* 162 */ {"var162", "undefined"},
      /* 163 */ {"var163", "undefined"},
      /* 164 */ {"var164", "undefined"},
      /* 165 */ {"var165", "undefined"},
      /* 166 */ {"var166", "undefined"},
      /* 167 */ {"var167", "undefined"},
      /* 168 */ {"var168", "undefined"},
      /* 169 */ {"var169", "undefined"},
      /* 170 */ {"var170", "undefined"},
      /* 171 */ {"var171", "undefined"},
      /* 172 */ {"var172", "undefined"},
      /* 173 */ {"var173", "undefined"},
      /* 174 */ {"var174", "undefined"},
      /* 175 */ {"var175", "undefined"},
      /* 176 */ {"var176", "undefined"},
      /* 177 */ {"var177", "undefined"},
      /* 178 */ {"var178", "undefined"},
      /* 179 */ {"var179", "undefined"},
      /* 180 */ {"var180", "undefined"},
      /* 181 */ {"var181", "undefined"},
      /* 182 */ {"var182", "undefined"},
      /* 183 */ {"var183", "undefined"},
      /* 184 */ {"var184", "undefined"},
      /* 185 */ {"var185", "undefined"},
      /* 186 */ {"var186", "undefined"},
      /* 187 */ {"var187", "undefined"},
      /* 188 */ {"var188", "undefined"},
      /* 189 */ {"var189", "undefined"},
      /* 190 */ {"var190", "undefined"},
      /* 191 */ {"var191", "undefined"},
      /* 192 */ {"var192", "undefined"},
      /* 193 */ {"var193", "undefined"},
      /* 194 */ {"var194", "undefined"},
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
      /* 254 */ {"var254", "undefined"},
      /* 255 */ {"var255", "undefined"},
};