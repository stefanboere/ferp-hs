pub use truck_js;
use truck_js::Solid;
use truck_param_base::{torus, Torus};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn build_torus(radius_major: f64, radius_minor: f64) -> Solid {
    torus(Torus::new(radius_major, radius_minor)).into()
}
