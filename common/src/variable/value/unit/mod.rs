mod distance;
mod mass;
mod temperature;

use std::fmt;

pub use distance::DistanceUnit;
pub use mass::MassUnit;
pub use temperature::TemperatureUnit;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Unit {
    Distance(DistanceUnit),
    Mass(MassUnit),
    Temperature(TemperatureUnit),
}

impl Unit {
    pub fn get_type_string(&self) -> &'static str {
        match self {
            Unit::Distance(_) => "distance",
            Unit::Mass(_) => "mass",
            Unit::Temperature(_) => "temperature",
        }
    }
}

impl fmt::Display for Unit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Unit::Distance(distance_unit) => write!(f, "{distance_unit}"),
            Unit::Mass(mass_unit) => write!(f, "{mass_unit}"),
            Unit::Temperature(temperature_unit) => write!(f, "{temperature_unit}"),
        }
    }
}
