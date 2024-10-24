mod distance;
mod mass;
mod temperature;
mod storage;

use std::fmt;

pub use distance::DistanceUnit;
pub use mass::MassUnit;
pub use storage::StorageUnit;
pub use temperature::TemperatureUnit;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Unit {
    Distance(DistanceUnit),
    Mass(MassUnit),
    Temperature(TemperatureUnit),
    Storage(StorageUnit),
}

impl Unit {
    pub fn get_type_string(&self) -> &'static str {
        match self {
            Unit::Distance(_) => "distance",
            Unit::Mass(_) => "mass",
            Unit::Temperature(_) => "temperature",
            Unit::Storage(_) => "torage"
        }
    }
}

impl fmt::Display for Unit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Unit::Distance(distance_unit) => write!(f, "{distance_unit}"),
            Unit::Mass(mass_unit) => write!(f, "{mass_unit}"),
            Unit::Temperature(temperature_unit) => write!(f, "{temperature_unit}"),
            Unit::Storage(storage_unit) => write!(f, "{storage_unit}")
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{DistanceUnit, MassUnit, TemperatureUnit, Unit};

    #[test]
    fn test_unit_get_type_string() {
        for (unit, expected) in [
            (Unit::Distance(DistanceUnit::Meter), "distance"),
            (Unit::Mass(MassUnit::Kilogram), "mass"),
            (Unit::Temperature(TemperatureUnit::Kelvin), "temperature"),
        ] {
            assert_eq!(unit.get_type_string(), expected);
        }
    }

    #[test]
    fn test_unit_display() {
        for (unit, expected) in [
            (Unit::Distance(DistanceUnit::Meter), "m"),
            (Unit::Mass(MassUnit::Kilogram), "kg"),
            (Unit::Temperature(TemperatureUnit::Kelvin), "°K"),
        ] {
            assert_eq!(unit.to_string(), expected);
        }
    }
}
