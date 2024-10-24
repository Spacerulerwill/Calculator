use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum DistanceUnit {
    // Bytes
    Nanometer,
    Micrometer,
    Millimeter,
    Centimeter,
    Meter,
    Kilometer,
    // Imperial
    Inch,
    Foot,
    Yard,
    Mile,
}

impl fmt::Display for DistanceUnit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            DistanceUnit::Nanometer => write!(f, "nm"),
            DistanceUnit::Micrometer => write!(f, "μm"),
            DistanceUnit::Millimeter => write!(f, "mm"),
            DistanceUnit::Centimeter => write!(f, "cm"),
            DistanceUnit::Meter => write!(f, "m"),
            DistanceUnit::Kilometer => write!(f, "km"),
            DistanceUnit::Inch => write!(f, "in"),
            DistanceUnit::Foot => write!(f, "ft"),
            DistanceUnit::Yard => write!(f, "yd"),
            DistanceUnit::Mile => write!(f, "mi"),
        }
    }
}

impl DistanceUnit {
    /// How many of this unit of distance is there per meter
    pub fn get_per_meter(&self) -> f64 {
        match self {
            DistanceUnit::Nanometer => 1e9,
            DistanceUnit::Micrometer => 1e6,
            DistanceUnit::Millimeter => 1e3,
            DistanceUnit::Centimeter => 100.0,
            DistanceUnit::Meter => 1.0,
            DistanceUnit::Kilometer => 1e-3,
            DistanceUnit::Inch => 39.3701,
            DistanceUnit::Foot => 3.28084,
            DistanceUnit::Yard => 1.0936133333333,
            DistanceUnit::Mile => 0.00062137121212119323429,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::DistanceUnit;

    #[test]
    fn test_display_distance_unit() {
        for (unit, expected) in [
            (DistanceUnit::Nanometer, "nm"),
            (DistanceUnit::Micrometer, "μm"),
            (DistanceUnit::Millimeter, "mm"),
            (DistanceUnit::Centimeter, "cm"),
            (DistanceUnit::Meter, "m"),
            (DistanceUnit::Kilometer, "km"),
            (DistanceUnit::Inch, "in"),
            (DistanceUnit::Foot, "ft"),
            (DistanceUnit::Yard, "yd"),
            (DistanceUnit::Mile, "mi"),
        ] {
            assert_eq!(unit.to_string(), expected);
        }
    }

    #[test]
    fn test_get_per_meter() {
        for (unit, per_meter) in [
            (DistanceUnit::Nanometer, 1e9),
            (DistanceUnit::Micrometer, 1e6),
            (DistanceUnit::Millimeter, 1e3),
            (DistanceUnit::Centimeter, 100.0),
            (DistanceUnit::Meter, 1.0),
            (DistanceUnit::Kilometer, 1e-3),
            (DistanceUnit::Inch, 39.3701),
            (DistanceUnit::Foot, 3.28084),
            (DistanceUnit::Yard, 1.0936133333333),
            (DistanceUnit::Mile, 0.00062137121212119323429),
        ] {
            assert_eq!(unit.get_per_meter(), per_meter)
        }
    }
}
