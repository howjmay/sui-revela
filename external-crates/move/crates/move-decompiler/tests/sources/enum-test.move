module 0x12::enum_only {
    public struct Test {
        a: u64,
        b: u64,
    }

    public enum Color has drop {
        RGB { red: u8, green: u8, blue: u8 },
        HSL { hue: u16, saturation: u8, lightness: u8 },
        Hex(u32),
    }

    public enum Option<T> {
        None,
        Some(T),
    }

    public fun is_rgb_color(color: Color): bool {
        match (color) {
            Color::RGB{ .. } => true,
            _ => false,
        }
    }
    public fun hex_color(color: Color): u32 {
        match (color) {
            Color::RGB{ red, green, blue } => (red as u32) << 16 | (green as u32) << 8 | (blue as u32),
            Color::HSL {..} => abort 0,
            Color::Hex(x) => x,
        }
    }
    public fun hex_color_ref(color: &Color): u32 {
        match (color) {
            Color::RGB{ red, green, blue } => (*red as u32) << 16 | (*green as u32) << 8 | (*blue as u32),
            Color::HSL {..} => abort 0,
            Color::Hex(x) => *x,
        }
    }
    const EOptionIsNone: u64 = 0;
    public fun unwrap_some<T>(option: Option<T>): T {
        match (option) {
            Option::Some(x) => x,
            Option::None => abort EOptionIsNone,
        }
    }
}
