(* Cardelli2005 plant vacuole *)
let plant_vacuole = "
let proton_pump = !exch(:atp)()=>(:adp, :p)(:hplus, :hminus).() in 
let ion_channel = !exch(:clminus)(:hplus)=>()(:hplus, :clminus).() in
let proton_antiporter = !exch(:naplus)(:hplus)=>(:hplus)(:naplus).() in 
let plant_vacuole = (proton_pump, ion_channel, proton_antiporter)[] in
plant_vacuole, :atp, :clminus
"

(* Cardelli2005 virus without molecules *)
let virus_lite = "
let nucap = (!bud.())[] in
let virus = (phago.(exo.()))[nucap] in
let membrane = !cophago(mate.()).(), !coexo. () in
let cytosol = (!comate.(), !coexo.())[] in
let cell = (membrane)[cytosol] in
let viral_envelope = cobud(phago.(exo.())).() in
let envelope_vesicle = (exo.(viral_envelope))[] in
virus, cell
"

(* Cardelli2005 virus with molecules *)
let virus = "
let disasm = exch(:trigger)(:vrna)=>(:vrna)().() in
let capsid = !bud.(), disasm in
let nucap = (capsid)[:vrna] in

let vrna_repl = (!exch(:vrna)()=>(:vrna, :vrna)().())[] in

let capsomers = exch(:vrna)()=>()(:vrna).(capsid) in
let capsomer_tran = (!exch(:vrna)()=>(:vrna)().(drip(capsomers).()))[] in

let viral_envelope = cobud(phago.(exo.())).() in
let envelope_vesicle = (exo.(viral_envelope))[] in

let er = (!exch(:vrna)()=>(:vrna)().(drip(exo.(viral_envelope)).()))[] in

let virus = (phago.(exo.()))[nucap] in
let membrane = !cophago(mate.()).(), !coexo. () in
let endosome = (!comate.(), !coexo.())[] in
let cell = (membrane)[endosome, :trigger, vrna_repl, capsomer_tran, er] in

virus, cell
"

(* Cardelli2005 virus with names *)
let virus_named = "
let disasm = exch(:trigger)(:vrna)=>(:vrna)().() in
let capsid = !bud{a}.(), disasm in
let nucap = (capsid)[:vrna] in

let vrna_repl = (!exch(:vrna)()=>(:vrna, :vrna)().())[] in

let capsomers = exch(:vrna)()=>()(:vrna).(capsid) in
let capsomer_tran = (!exch(:vrna)()=>(:vrna)().(drip(capsomers).()))[] in

let viral_envelope = cobud{a}(phago.(exo.())).() in
let envelope_vesicle = (exo.(viral_envelope))[] in

let er = (!exch(:vrna)()=>(:vrna)().(drip(exo.(viral_envelope)).()))[] in

let virus = (phago.(exo.()))[nucap] in
let membrane = !cophago(mate{b}.()).(), !coexo. () in
let endosome = (!comate{b}.(), !coexo.())[] in
let cell = (membrane)[endosome, :trigger, vrna_repl, capsomer_tran, er] in

virus, cell
"

(* Degano2006 enzyme mutex (inhibitable catalyst) *)
let enzyme_mutex = "
let enzyme = (
  !exch(:a)(:free)=>()(:occ_a).(),
  !exch(:b)(:free)=>()(:occ_b).(),
  !exch()(:occ_a)=>(:a)(:free).(),
  !exch()(:occ_b)=>(:b)(:free).()
)[:free] in
:a, :b, enzyme
"
