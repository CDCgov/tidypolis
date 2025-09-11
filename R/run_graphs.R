# Master Graph Test

source(here::here("R/create_new_sia_table.R"))
source(here::here("R/run_obx_graphs.R"))

run_obx_sia_graph(
  raw.data = raw.data,
  obx_table = obx_table,
  sia_full_all = sia_full_all,
  sia_obx_table = sia_obx_table,
  run_type = "all",
  save_graph = "yes",
  cdc_upload = "yes",
  viz = "sen_check"
)


emg_cols = c(
  "AFG-HLD-1" = "#00fa9a",
  "AFG-NGR-1" = "#20b2aa",

  "ANG-BEN-1" = "#458B74",
  "ANG-BEN-2" = "#8B3E2F",
  "ANG-BEN-3" = "#458B00",
  "ANG-HUI-1" = "#ffc40c",
  "ANG-LNO-1" = "#6495ed",
  "ANG-LNO-2" = "#ee82ee",
  "ANG-LNO-3" = "#68228b",
  "ANG-LUA-1" = "#ff9f00",
  "ANG-MOX-1" = "#cc5500",

  "BOT-FRA-1" = "#458B74",

  "CAE-EST-1" = "#B254A5",
  "CAE-EXT-1" = "#fd8d3c",

  "CAF-BAM-1" = "#fa8072",
  "CAF-BAM-2" = "#ffbf00",
  "CAF-BER-1" = "#ffe4e1",
  "CAF-BIM-1" = "#367588",
  "CAF-BIM-2" = "#00fa9a",
  "CAF-BIM-3" = "#ccff00",
  "CAF-BNG-1" = "#78184a",
  "CAF-BNG-2" = "#d21404",
  "CAF-BNG-3" = "#00008b",
  "CAF-KEM-1" = "#43b3ae",
  "CAF-MOZ-1" = "#b22222",

  "CHA-NDJ-1" = "#68228b",

  "CHN-SHA-1" = "#e6e6fa",
  "CHN-SIC-1" = "#e86100",

  "DJI-DJI-1" = "#05998c",

  "EGY-NOR-1" = "#8B2323",
  "EGY-QEN-1" = "#48d1cc",

  "ETH-ORO-1" = "#efcc00",
  "ETH-ORO-2" = "#ff6700",
  "ETH-ORO-3" = "#e63e62",
  "ETH-ORO-4" = "#483d8b",
  "ETH-ORO-5" = "#458B00",
  "ETH-SOM-1" = "#bc8f8f",
  "ETH-SOU-1" = "#6495ed",
  "ETH-SOU-2" = "#8fbc8f",
  "ETH-SOU-3" = "#ffdab9",
  "ETH-TIG-1" = "#CA5621",

  "GUF-SGO-1" = "#458B00",

  "GUI-KAN-1" = "#FF6633",

  "INO-ACE-1" = "#ED7222",
  'INO-PAP-1' = "#ffc40c",
  "INO-PAP-2" = "#458B00",

  "ISR-JER-1" = "#05998c",

  "IUUC-2022" = "#7c0a02",

  "MAD-ANO-1" = "#228b22",
  "MAD-ANO-2" = "#B254A5",
  "MAD-SUE-1" = "#fd5800",
  "MAD-SUO-1" = "#5f9ea0",

  "MMR-1" = "#e9967a",

  "MOZ-MAN-1" = "#458B00",
  "MOZ-NPL-1" = "#fe5a1d",
  "MOZ-NPL-2" = "#008B8b",
  "MOZ-ZAM-2" = "#d8bfd8",

  "NIE-BOS-1" = "#104E8B",
  "NIE-JIS-1" = "#87ceeb",
  "NIE-KBS-1" = "#ff4500",
  "NIE-KGS-1" = "#008000",
  "NIE-KGS-2" = "#adff2f",
  "NIE-KTS-1" = "#68228b",
  "NIE-SOS-2" = "#ff7f50",
  "NIE-SOS-3" = "#ffbf00",
  "NIE-SOS-4" = "#d7837f",
  "NIE-SOS-5" = "#ffc1cc",
  "NIE-SOS-6" = "#bc8f8f",
  "NIE-SOS-7" = "#ffa07a",
  "NIE-SOS-8" = "#efcc00",
  "NIE-YBS-1" = "#05998c",
  "NIE-YBS-2" = "#8B0A50",
  "NIE-ZAS-1" = "#F5191C",

  "NIG-DOS-1" = "#B254A5",

  "PAK-BN-RP-1" = "#104E8B",
  "PAK-BN-TW-1" = "#ffa07a",
  "PAK-FSD-1" = "#ff4500",
  "PAK-FSD-2" = "#008000",
  "PAK-GB-1" =  "#F5191C",
  "PAK-GB-2" = "#68228b",
  "PAK-GB-3" = "#adff2f",
  "PAK-KAM-1" = "#8B0A50",
  "PAK-KOH-1" = "#87ceeb",
  "PAK-LKW-1" = "#efcc00",
  "PAK-PB-1" = "#8B8B83",
  "PAK-PWR-1" ="#d8bfd8",
  "PAK-QTA-1" =  "#05998c",
  "PAK-TOR-1" = "#8B0A50",
  "PAK−KHI−2" = "#ff7f50",

  "PHL-NCR-1" = "#ec5800",
  "PHL-NCR-2" = "#009b7d",

  "PNG-MOR-1" = "#355e3b",

  "RDC-BUE-1" = "#8B8B83",
  "RDC-EQT-1" = "#B254A5",
  "RDC-HKA-1" = "#008000",
  "RDC-HKA-2" = "#B14A34",
  "RDC-HKA-3" = "#104E8B",
  "RDC-HLO-1" = "#efcc00",
  "RDC-HLO-2" = "#ffa07a",
  "RDC-HLO-3" = "#458B00",
  "RDC-HLO-4" = "#ffc1cc",
  "RDC-KAS-1" = "#d8bfd8",
  "RDC-KAS-1" = "#480607",
  "RDC-KAS-2" = "#ce2029",
  "RDC-KAS-3" = "#458B00",
  "RDC-KOR-1" = "#104E8B",
  "RDC-MAN-1" = "#a6e7ff",
  "RDC-MAN-2" = "#dc143c",
  "RDC-MAN-3" = "#556B2F",
  "RDC-MAN-4" = "#e6e6fa",
  "RDC-MAN-5" = "#B5651d",
  "RDC-MON-1" = "#682860",
  "RDC-SAN-1" = "#ff4f00",
  "RDC-SKV-1" = "#8B3A3A",
  "RDC-TAN-1" = "#CA5621",
  "RDC-TAN-2" = "#00cccc",
  "RDC-TPA-1" = "#adff2f",
  "RDC-TPA-2" = "#00ffff",
  "RDC-TSH-1" = "#AC6A9F",
  "RDC-TSH-2" = "#F5191C",
  "RDC-TSH-3" = "#4A708B",

  "RSS-CEQ-1" = "#00cccc",
  "RSS-JON-1" = "#fd8d3c",
  "RSS-UNL-1" = "#2D7E47",
  "RSS-WEQ-1" = "#8B8B83",

  "SOM-AWL-1" = "#00ffff",
  "SOM-BAN-1" = "#891171",
  "SOM-BAN-2" = "#efcc00",
  "SOM-BAY-1" = "#7C68B3",

  "SUD-RED-1" = "#F28265",

  "SYR-DEI-1" = "#ffa07a",

  "TAN-MWA-1" = "#669933",

  "TOG-SAV-1" = "#cb4154",

  "VDPV 1" = "#000000",
  "VDPV 2" = "#000000",
  "VDPV 3" = "#000000",

  "YEM-SAD-1" = "#cb4154",
  "YEM-SAN-1" = "#ff4500",
  "YEM-TAI-1" = "#4A708B",

  "ZAM-LUA-1" = "#800080",

  "ZIM-HRE-1" = "#A03E3F",
  "ZIM-HRE-2" = "#2D7E47",

  "YB3A" = "#4A708B",
  "YB3C" = "#A03E3F",
  "YB3A4A" = "#AC6A9F",
  "YB3A4A & YB3A4B" = "#fd8d3c",
  "YB3A4B" = "#2D7E47",

  "cVDPV-A" = "#6495ed",

  "cVDPV2" = "grey80",
  "cVDPV3" = "grey80",
  "EGY-3" = "grey80",
  "CHA-cVDPV2" = "grey80",
  "cVDPV2-RDC" = "grey80",
  "cVDPV3-ISR" = "grey80",
  "INO-cVDPV2" = "grey80",
  "cVDPV2-ETH" = "grey80",
  "ANG-cVDPV2" = "grey80",
  "RSS-cVDPV2" = "grey80",
  "TAN-cVDPV2" = "grey80"


)
