use serde::{Deserialize, Serialize};
use std::f64::consts::PI;
use truck_modeling::*;

#[derive(Serialize, Deserialize, Debug)]
pub struct Torus {
    radius_major: f64,
    radius_minor: f64,
}

impl Torus {
    pub fn new(radius_major: f64, radius_minor: f64) -> Self {
        Self {
            radius_major,
            radius_minor,
        }
    }
}

impl Default for Torus {
    fn default() -> Self {
        Torus {
            radius_major: 0.75,
            radius_minor: 0.25,
        }
    }
}

pub fn torus(x: Torus) -> Solid {
    let v = builder::vertex(Point3::new(x.radius_major, 0.0, x.radius_minor));
    let w = builder::rsweep(
        &v,
        Point3::new(x.radius_major, 0.0, 0.0),
        Vector3::unit_y(),
        Rad(7.0),
    );
    let shell = builder::rsweep(&w, Point3::origin(), Vector3::unit_z(), Rad(7.0));
    Solid::new(vec![shell])
}

#[derive(Serialize, Deserialize, Debug)]
pub struct Klupper {
    outside_diameter: f64,
    wall_thickness: f64,
    straight_flange_height: f64,
}

impl Default for Klupper {
    fn default() -> Self {
        Klupper {
            outside_diameter: 1.0,
            wall_thickness: 0.010,
            straight_flange_height: 0.050,
        }
    }
}

pub fn klupper(x: Klupper) -> Shell {
    let outside_radius = x.outside_diameter / 2.0;
    let crown_radius = x.outside_diameter;
    let knucke_radius = 0.1 * x.outside_diameter;

    let cos_knuckle_angle =
        (outside_radius - x.wall_thickness - knucke_radius) / (crown_radius - knucke_radius);
    let knuckle_angle = cos_knuckle_angle.acos();

    // The straight flange part
    let straight_flange_bottom_outer: Vertex =
        builder::vertex(Point3::new(0.0, outside_radius, -x.straight_flange_height));
    let flange_top_right: Vertex = builder::vertex(Point3::new(0.0, outside_radius, 0.0));

    let mut wire: Wire = Wire::new();
    wire.push_front(builder::line(
        &straight_flange_bottom_outer,
        &flange_top_right,
    ));

    // The torical part
    let tori_center: Point3 =
        Point3::new(0.0, outside_radius - knucke_radius - x.wall_thickness, 0.0);
    let mut flange_circle: Wire = builder::rsweep(
        &flange_top_right,
        tori_center,
        Vector3::unit_x(),
        Rad(knuckle_angle),
    );
    wire.append(&mut flange_circle);

    let dished_height = crown_radius * (1.0 - knuckle_angle.sin())
        + knucke_radius * knuckle_angle.sin()
        + x.wall_thickness;

    let circle_angle = PI / 2.0 - knuckle_angle;
    let circle_start = wire.back_vertex().unwrap();
    // We must leave a hole in the top because rsweep cannot handle fixed points yet
    let mut top_circle: Wire = builder::rsweep(
        circle_start,
        Point3::new(0.0, 0.0, dished_height - crown_radius),
        Vector3::unit_x(),
        Rad(0.90 * circle_angle),
    );

    wire.append(&mut top_circle);

    builder::rsweep(&wire, Point3::origin(), Vector3::unit_z(), Rad(7.0))
}

#[cfg(test)]
mod tests {
    use super::*;
}
