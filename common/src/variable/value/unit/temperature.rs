use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TemperatureUnit {
    Kelvin,
    Celsius,
    Fahrenheit,
}

impl fmt::Display for TemperatureUnit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TemperatureUnit::Celsius => write!(f, "°C"),
            TemperatureUnit::Kelvin => write!(f, "°K"),
            TemperatureUnit::Fahrenheit => write!(f, "°F"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::TemperatureUnit;

    #[test]
    fn test_display_temperature_unit() {
        for (unit, expected) in [
            (TemperatureUnit::Celsius, "°C"),
            (TemperatureUnit::Kelvin, "°K"),
            (TemperatureUnit::Fahrenheit, "°F"),
        ] {
            assert_eq!(unit.to_string(), expected)
        }
    }
}
