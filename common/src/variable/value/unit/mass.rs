use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum MassUnit {
    // Metric
    Nanogram,
    Microgram,
    Milligram,
    Gram,
    Kilogram,
    Tonne,
    // Imperial
    Ounce,
    Pound,
    Stone,
}

impl fmt::Display for MassUnit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MassUnit::Nanogram => write!(f, "ng"),
            MassUnit::Microgram => write!(f, "µg"),
            MassUnit::Milligram => write!(f, "mg"),
            MassUnit::Gram => write!(f, "g"),
            MassUnit::Kilogram => write!(f, "kg"),
            MassUnit::Tonne => write!(f, "t"),
            MassUnit::Ounce => write!(f, "oz"),
            MassUnit::Pound => write!(f, "lb"),
            MassUnit::Stone => write!(f, "st"),
        }
    }
}

impl MassUnit {
    /// How many of this unit of mass there are per kilograms
    pub fn get_per_kilo(&self) -> f64 {
        match self {
            MassUnit::Nanogram => 1e12,
            MassUnit::Microgram => 1e9,
            MassUnit::Milligram => 1e6,
            MassUnit::Gram => 1e3,
            MassUnit::Kilogram => 1.0,
            MassUnit::Tonne => 1e-3,
            MassUnit::Ounce => 35.274,
            MassUnit::Pound => 2.20462,
            MassUnit::Stone => 0.157473,
        }
    }
}
