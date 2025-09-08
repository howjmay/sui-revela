module std::Functions {
    public fun f1() {
    }

    public fun f2(x: u8, y: u8): u8 {
        0
    }

    public fun f3(x: &u8, y: &mut u8, z: u8): u8 {
        *y = *x + z;
        *y
    }

    public fun f4(): (u8, u16, u32) {
        (8,16,32)
    }

    public struct Coin<phantom CoinType> has store {
        value: u64
    }

    public struct Coin2<phantom CoinType> has store {
        value: u64,
        value2: u64
    }

    public struct Balance<phantom CoinType> has key {
        coin: Coin<CoinType>
    }

    fun test_abort(): u8 {
        abort 2
    }
}
