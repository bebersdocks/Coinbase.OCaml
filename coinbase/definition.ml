exception Unmatched_definition of string

type entry = 
  | Transfer 
  | Match 
  | Fee 
  | Rebate 
  | Conversion

let string_of_entry entry = 
 match entry with 
  | Transfer -> "transfer"
  | Match -> "match"
  | Fee -> "fee"
  | Rebate -> "rebate"
  | Conversion -> "conversion"

let entry_of_string entry =
  match entry with
  | "transfer" -> Transfer
  | "match" -> Match
  | "fee" -> Fee
  | "rebate" -> Rebate
  | "conversion" -> Conversion
  | _ -> raise (Unmatched_definition ("entry_of_string: '" ^ entry ^ "' entry."))

type hold = 
  | Order 
  | Transfer 

let string_of_hold hold = 
  match hold with
  | Order -> "order"
  | Transfer -> "transfer"

let hold_of_string hold = 
  match hold with 
  | "order" -> Order 
  | "transfer" -> Transfer
  | _ -> raise (Unmatched_definition ("hold_of_string: '" ^ hold ^ "' hold."))
  
type product =
  | ALGO_USD
  | ATOM_BTC 
  | ATOM_USD
  | BAT_ETH 
  | BAT_USDC
  | BCH_BTC 
  | BCH_EUR 
  | BCH_GBP
  | BTC_EUR 
  | BTC_GBP 
  | BTC_USD
  | BTC_USDC 
  | CVC_USDC
  | DAI_USD 
  | DAI_USDC 
  | DASH_BTC 
  | DASH_USD
  | DNT_USDC
  | EOS_BTC 
  | EOS_EUR 
  | EOS_USD
  | ETC_BTC 
  | ETC_EUR
  | ETC_GBP
  | ETC_USD
  | ETH_BTC 
  | ETH_DAI
  | ETH_EUR
  | ETH_GBP
  | ETH_USD
  | ETH_USDC  
  | GNT_USDC
  | KNC_BTC 
  | KNC_USD
  | LINK_ETH 
  | LINK_USD
  | LOOM_USDC
  | LTC_BTC
  | LTC_EUR
  | LTC_GBP
  | LTC_USD
  | MANA_USDC
  | MKR_BTC
  | MKR_USD
  | OMG_BTC 
  | OMG_EUR 
  | OMG_GBP 
  | OMG_USD
  | OXT_USD 
  | REP_BTC 
  | REP_USD
  | XLM_BTC 
  | XLM_EUR 
  | XLM_USD
  | XRP_BTC 
  | XRP_EUR 
  | XRP_USD
  | XTZ_BTC 
  | XTZ_USD
  | ZEC_BTC
  | ZEC_USDC 
  | ZRX_BTC 
  | ZRX_EUR 
  | ZRX_USD

let string_of_product product =
  match product with
  | ALGO_USD -> "ALGO-USD"
  | ATOM_BTC -> "ATOM-BTC"
  | ATOM_USD -> "ATOM-USD"
  | BAT_ETH -> "BAT-ETH"
  | BAT_USDC -> "BAT-USDC"
  | BCH_BTC -> "BCH-BTC"
  | BCH_EUR -> "BCH-EUR"
  | BCH_GBP -> "BCH-GBP"
  | BTC_EUR -> "BTC-EUR"
  | BTC_GBP -> "BTC-GBP"
  | BTC_USD -> "BTC-USD"
  | BTC_USDC -> "BTC-USDC"
  | CVC_USDC -> "CVC-USDC"
  | DAI_USD -> "DAI-USD"
  | DAI_USDC -> "DAI-USDC"
  | DASH_BTC -> "DASH-BTC"
  | DASH_USD -> "DASH-USD"
  | DNT_USDC -> "DNT-USDC"
  | EOS_BTC -> "EOS-BTC"
  | EOS_EUR -> "EOS-EUR"
  | EOS_USD -> "EOS-USD"
  | ETC_BTC -> "ETC-BTC"
  | ETC_EUR -> "ETC-EUR"
  | ETC_GBP -> "ETC-GBP"
  | ETC_USD -> "ETC-USD"
  | ETH_BTC -> "ETH-BTC"
  | ETH_DAI -> "ETH-DAI"
  | ETH_EUR -> "ETH-EUR"
  | ETH_GBP -> "ETH-GBP"
  | ETH_USD -> "ETH-USD"
  | ETH_USDC -> "ETH-USDC"
  | GNT_USDC -> "GNT-USDC"
  | KNC_BTC -> "KNC-BTC"
  | KNC_USD -> "KNC-USD"
  | LINK_ETH -> "LINK-ETH"
  | LINK_USD -> "LINK-USD"
  | LOOM_USDC -> "LOOM-USDC"
  | LTC_BTC -> "LTC-BTC"
  | LTC_EUR -> "LTC-EUR"
  | LTC_GBP -> "LTC-GBP"
  | LTC_USD -> "LTC-USD"
  | MANA_USDC -> "MANA-USDC"
  | MKR_BTC -> "MKR-BTC"
  | MKR_USD -> "MKR-USD"
  | OMG_BTC -> "OMG-BTC"
  | OMG_EUR -> "OMG-EUR"
  | OMG_GBP -> "OMG-GBP"
  | OMG_USD -> "OMG-USD"
  | OXT_USD -> "OXT-USD"
  | REP_BTC -> "REP-BTC"
  | REP_USD -> "REP-USD"
  | XLM_BTC -> "XLM-BTC"
  | XLM_EUR -> "XLM-EUR"
  | XLM_USD -> "XLM-USD"
  | XRP_BTC -> "XRP-BTC"
  | XRP_EUR -> "XRP-EUR"
  | XRP_USD -> "XRP-USD"
  | XTZ_BTC -> "XTZ-BTC"
  | XTZ_USD -> "XTZ-USD"
  | ZEC_BTC -> "ZEC-BTC"
  | ZEC_USDC -> "ZEC-USDC"
  | ZRX_BTC -> "ZRX-BTC"
  | ZRX_EUR -> "ZRX-EUR"
  | ZRX_USD -> "ZRS-USD"
        
let product_of_string product_id =
  match product_id with
  | "ALGO-USD" -> ALGO_USD
  | "ATOM-BTC" -> ATOM_BTC
  | "ATOM-USD" -> ATOM_USD
  | "BAT-ETH" -> BAT_ETH
  | "BAT-USDC" -> BAT_USDC
  | "BCH-BTC" -> BCH_BTC
  | "BCH-EUR" -> BCH_EUR
  | "BCH-GBP" -> BCH_GBP
  | "BTC-EUR" -> BTC_EUR
  | "BTC-GBP" -> BTC_GBP
  | "BTC-USD" -> BTC_USD
  | "BTC-USDC" -> BTC_USDC
  | "CVC-USDC" -> CVC_USDC
  | "DAI-USD" -> DAI_USD
  | "DAI-USDC" -> DAI_USDC
  | "DASH-BTC" -> DASH_BTC
  | "DASH-USD" -> DASH_USD
  | "DNT-USDC" -> DNT_USDC
  | "EOS-BTC" -> EOS_BTC
  | "EOS-EUR" -> EOS_EUR
  | "EOS-USD" -> EOS_USD
  | "ETC-BTC" -> ETC_BTC
  | "ETC-EUR" -> ETC_EUR
  | "ETC-GBP" -> ETC_GBP
  | "ETC-USD" -> ETC_USD
  | "ETH-BTC" -> ETH_BTC
  | "ETH-DAI" -> ETH_DAI
  | "ETH-EUR" -> ETH_EUR
  | "ETH-GBP" -> ETH_GBP
  | "ETH-USD" -> ETH_USD
  | "ETH-USDC" -> ETH_USDC
  | "GNT-USDC" -> GNT_USDC
  | "KNC-BTC" -> KNC_BTC
  | "KNC-USD" -> KNC_USD
  | "LINK-ETH" -> LINK_ETH
  | "LINK-USD" -> LINK_USD
  | "LOOM-USDC" -> LOOM_USDC
  | "LTC-BTC" -> LTC_BTC
  | "LTC-EUR" -> LTC_EUR
  | "LTC-GBP" -> LTC_GBP
  | "LTC-USD" -> LTC_USD
  | "MANA-USDC" -> MANA_USDC
  | "MKR-BTC" -> MKR_BTC
  | "MKR-USD" -> MKR_USD
  | "OMG-BTC" -> OMG_BTC
  | "OMG-EUR" -> OMG_EUR
  | "OMG-GBP" -> OMG_GBP
  | "OMG-USD" -> OMG_USD
  | "OXT-USD" -> OXT_USD
  | "REP-BTC" -> REP_BTC
  | "REP-USD" -> REP_USD
  | "XLM-BTC" -> XLM_BTC
  | "XLM-EUR" -> XLM_EUR
  | "XLM-USD" -> XLM_USD
  | "XRP-BTC" -> XRP_BTC
  | "XRP-EUR" -> XRP_EUR
  | "XRP-USD" -> XRP_USD
  | "XTZ-BTC" -> XTZ_BTC
  | "XTZ-USD" -> XTZ_USD
  | "ZEC-BTC" -> ZEC_BTC
  | "ZEC-USDC" -> ZEC_USDC
  | "ZRX-BTC" -> ZRX_BTC
  | "ZRX-EUR" -> ZRX_EUR
  | "ZRX-USD" -> ZRX_USD
  | _ -> raise (Unmatched_definition ("product_of_string: '" ^ product_id ^ "' product id."))

type side =
  | Buy
  | Sell

let string_of_side side =
  match side with
  | Buy -> "buy"
  | Sell -> "sell"
          
let side_of_string side =
  match side with
  | "buy" -> Buy
  | "sell" -> Sell
  | _ -> raise (Unmatched_definition ("side_of_string: '" ^ side ^ "' side."))
      
type order = 
  | Limit 
  | Market

let string_of_order order = 
  match order with 
  | Limit -> "limit"
  | Market -> "market"

let order_of_string order =
  match order with 
  | "limit" -> Limit 
  | "market" -> Market
  | _ -> raise (Unmatched_definition ("order_of_string: '" ^ order ^ "' order."))

type stop = 
  | None
  | Loss
  | Entry
    
let string_of_stop stop = 
  match stop with 
  | None -> ""
  | Loss -> "loss"
  | Entry -> "entry"
    
let stop_of_string stop =
  match stop with 
  | "loss" -> Loss
  | "entry" -> Entry
  | _ -> raise (Unmatched_definition ("stop_of_string: '" ^ stop ^ "' stop."))

type time_in_force = 
  | GTC 
  | GTT 
  | IOC 
  | FOK 

let string_of_time_in_force time_in_force =
  match time_in_force with 
  | GTC -> "GTC"
  | GTT -> "GTT"
  | IOC -> "IOC"
  | FOK -> "FOK"
  
let time_in_force_of_string time_in_force = 
  match time_in_force with 
  | "GTC" -> GTC 
  | "GTT" -> GTT 
  | "IOC" -> IOC 
  | "FOK" -> FOK
  | _ -> raise (Unmatched_definition ("time_in_force_of_string: '" ^ time_in_force ^ "' time in force."))

type status =
  | Open 
  | Done
  | Pending
  | Active

let string_of_status status =
  match status with
  | Open -> "open"
  | Done -> "done"
  | Pending -> "pending"
  | Active -> "active"
            
let status_of_string status =
  match status with
  | "open" -> Open
  | "done" -> Done
  | "pending" -> Pending
  | "active" -> Active
  | _ -> raise (Unmatched_definition ("status_of_string: '" ^ status ^ "' status."))           

type currency = 
  | ALGO 
  | ATOM 
  | BAT 
  | BCH 
  | BTC 
  | CVC
  | DAI 
  | DASH 
  | DNT
  | EOS
  | ETC
  | ETH  
  | EUR
  | GNT 
  | KNC 
  | LINK 
  | LOOM 
  | LTC
  | MANA 
  | MKR
  | OMG 
  | OXT 
  | REP
  | USDC
  | XLM
  | XRP
  | XTZ
  | ZEC 
  | ZRX 

let string_of_currency currency = 
  match currency with
  | ALGO -> "ALGO"
  | ATOM -> "ATOM"
  | BAT -> "BAT"
  | BCH -> "BCH"
  | BTC -> "BTC"
  | CVC -> "CVC"
  | DAI -> "DAI"
  | DASH -> "DASH"
  | DNT -> "DNT"
  | EOS -> "EOS"
  | ETC -> "ETC"
  | ETH -> "ETH"
  | EUR -> "EUR"
  | GNT -> "GNT"
  | KNC -> "KNC"
  | LINK -> "LINK"
  | LOOM -> "LOOM"
  | LTC -> "LTC"
  | MANA -> "MANA"
  | MKR -> "MKR"
  | OMG -> "OMG"
  | OXT -> "OXT"
  | REP -> "REP"
  | USDC -> "USDC"
  | XLM -> "XLM"
  | XRP -> "XRP"
  | XTZ -> "XTZ"
  | ZEC -> "ZEC"
  | ZRX -> "ZRX"

let currency_of_string currency = 
  match currency with 
  | "ALGO" -> ALGO
  | "ATOM" -> ATOM
  | "BAT" -> BAT
  | "BCH" -> BCH
  | "BTC" -> BTC
  | "CVC" -> CVC
  | "DAI" -> DAI
  | "DASH" -> DASH
  | "DNT" -> DNT
  | "EOS" -> EOS
  | "ETC" -> ETC
  | "ETH" -> ETH
  | "EUR" -> EUR
  | "GNT" -> GNT
  | "KNC" -> KNC
  | "LINK" -> LINK
  | "LOOM" -> LOOM
  | "LTC" -> LTC
  | "MANA" -> MANA
  | "MKR" -> MKR
  | "OMG" -> OMG
  | "OXT" -> OXT
  | "REP" -> REP
  | "USDC" -> USDC
  | "XLM" -> XLM
  | "XRP" -> XRP
  | "XTZ" -> XTZ
  | "ZEC" -> ZEC
  | "ZRX" -> ZRX
  | _ -> raise (Unmatched_definition("currency_of_string: '" ^ currency ^ "' currency."))

type liquidity = 
  | Maker
  | Taker

let string_of_liquidity liquidity = 
  match liquidity with 
  | Maker -> "M"
  | Taker -> "T"

let liquidity_of_string liquidity =
  match liquidity with 
  | "M" -> Maker
  | "T" -> Taker
  | _ -> raise (Unmatched_definition ("liquidity_of_string: '" ^ liquidity ^ "' liquidity."))

type self_trade_prevention =
  | DC 
  | CO 
  | CN 
  | CB

let string_of_self_trade_prevention self_trade_prevention = 
  match self_trade_prevention with 
  | DC -> "dc"
  | CO -> "co"
  | CN -> "cn"
  | CB -> "cb"

let self_trade_prevention_of_string self_trade_prevention = 
  match self_trade_prevention with 
  | "dc" -> DC 
  | "co" -> CO 
  | "cn" -> CN 
  | "cb" -> CB
  | _ -> raise (Unmatched_definition ("self_trade_prevention_of_string: '" ^ self_trade_prevention ^ "' stp."))