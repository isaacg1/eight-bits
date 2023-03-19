#![feature(map_try_insert)]
use std::collections::HashMap;

use integer_sqrt::IntegerSquareRoot;

#[derive(Eq, PartialEq, Hash, Clone, Copy, Debug)]
struct Key {
    num: u16,
    zeros: u8,
    ones: u8,
}
#[derive(Debug, Clone, Copy)]
enum Value {
    Direct(u8),
    Factorial(Key),
    DoubleFactorial(Key),
    Sqrt(Key),
    Plus(Key, Key),
    Minus(Key, Key),
    Times(Key, Key),
    TimesShift(Key, Key, u8),
    DivShift(Key, Key, u8),
    Div(Key, Key),
    Exp(Key, Key),
    Dominated,
}
fn search() {
    let mut table: HashMap<Key, Value> = HashMap::new();
    let limit = 65535;
    let mut new_pairs = vec![];
    for i in 0..255u8 {
        let zeros = if i == 0 {
            1
        } else {
            (i.count_zeros() - i.leading_zeros()) as u8
        };
        let ones = i.count_ones() as u8;
        if zeros <= 4 && ones <= 4 {
            let key = Key {
                num: i as u16,
                zeros,
                ones,
            };
            new_pairs.push((key, Value::Direct(i)));
        }
    }
    let mut table_count = 1;
    while table_count != table.len() {
        table_count = table.len();
        for (key, value) in new_pairs.drain(..) {
            if key.num > limit {
                continue;
            }
            let result = table.try_insert(key, value);
            if result.is_ok() {
                for more_zeros in key.zeros..=4 {
                    for more_ones in key.ones..=4 {
                        if more_zeros != key.zeros || more_ones != key.ones {
                            let new_key = Key {
                                num: key.num,
                                zeros: more_zeros,
                                ones: more_ones,
                            };
                            table.insert(new_key, Value::Dominated);
                        }
                    }
                }
            }
        }
        for (&key1, value1) in &table {
            assert!(key1.zeros <= 4);
            assert!(key1.ones <= 4);
            if matches!(value1, Value::Dominated) {
                continue;
            }
            let num1 = key1.num;
            let mut singles = vec![];
            if num1 <= 8 {
                let fact = (1..=num1).product();
                singles.push((fact, Value::Factorial(key1)));
            }
            if num1 <= 12 {
                let fact = (1..=num1).rev().step_by(2).product();
                singles.push((fact, Value::DoubleFactorial(key1)));
            }
            let isqrt = num1.integer_sqrt();
            if isqrt.pow(2) == num1 {
                singles.push((isqrt, Value::Sqrt(key1)));
            }
            for (num, value) in singles {
                let new_key = Key {
                    num,
                    zeros: key1.zeros,
                    ones: key1.ones,
                };
                new_pairs.push((new_key, value));
            }
            for (&key2, value2) in &table {
                if matches!(value2, Value::Dominated) {
                    continue;
                }
                let num2 = key2.num;
                let total_zeros = key1.zeros + key2.zeros;
                let total_ones = key1.ones + key2.ones;
                if total_zeros > 4 || total_ones > 4 {
                    continue;
                }
                if matches!(value2, Value::Direct(_)) {
                    for shift in 1..8u8 {
                        let extra_zeros = shift.saturating_sub(16 - num2.leading_zeros() as u8);
                        let new_zeros = total_zeros + extra_zeros;
                        if new_zeros > 4 {
                            continue;
                        }
                        if num1 % (1 << shift) == 0 {
                            if let Some(num) = num1.checked_mul(num2).map(|res| res >> shift) {
                                let new_key = Key {
                                    num,
                                    zeros: new_zeros,
                                    ones: total_ones,
                                };
                                new_pairs.push((new_key, Value::TimesShift(key1, key2, shift)));
                            }
                        }
                        if num2 != 0 && num1 % num2 == 0 {
                            if let Some(num) = (num1 / num2).checked_shl(shift.into()) {
                                let new_key = Key {
                                    num,
                                    zeros: new_zeros,
                                    ones: total_ones,
                                };
                                new_pairs.push((new_key, Value::DivShift(key1, key2, shift)));
                            }
                        }
                    }
                }
                if num1 < num2 {
                    continue;
                }
                let mut ops = vec![
                    (num1.checked_pow(num2.into()), Value::Exp(key1, key2)),
                    (num2.checked_pow(num1.into()), Value::Exp(key2, key1)),
                    (num1.checked_add(num2), Value::Plus(key1, key2)),
                    (num1.checked_sub(num2), Value::Minus(key1, key2)),
                    (num1.checked_mul(num2), Value::Times(key1, key2)),
                ];
                if num2 != 0 && num1 % num2 == 0 {
                    ops.push((Some(num1 / num2), Value::Div(key1, key2)));
                }
                for (maybe_num, value) in ops {
                    if let Some(num) = maybe_num {
                        let new_key = Key {
                            num,
                            zeros: total_zeros,
                            ones: total_ones,
                        };
                        new_pairs.push((new_key, value))
                    }
                }
            }
        }
    }
    'outer: for num in 0..=500 {
        for sum in 0..=8u8 {
            for zeros in (sum.saturating_sub(4)..=4).rev() {
                let ones = sum - zeros;
                let key = Key { num, zeros, ones };
                if let Some(value) = table.get(&key) {
                    if !matches!(value, Value::Dominated) {
                        println!(
                            "{}: {}",
                            key.num,
                            value_to_string(*value, &table, false)
                        );
                        continue 'outer
                    }
                }
            }
        }
        println!("{} was not found", num);
    }
}
fn value_to_string(value: Value, table: &HashMap<Key, Value>, wrap: bool) -> String {
    let unwrapped_string = match value {
        Value::Direct(num) => format!("{:b}", num),
        Value::Minus(key1, key2)
        | Value::Plus(key1, key2)
        | Value::Times(key1, key2)
        | Value::Div(key1, key2)
        | Value::Exp(key1, key2) => {
            let (op, wrap1, wrap2) = match value {
                Value::Minus(_, _) => ('-', false, true),
                Value::Plus(_, _) => ('+', false, false),
                Value::Times(_, _) => ('*', true, true),
                Value::Div(_, _) => ('/', true, true),
                Value::Exp(_, _) => ('^', true, true),
                _ => unreachable!(),
            };
            let value1 = *table.get(&key1).expect("Present");
            let value2 = *table.get(&key2).expect("Present");
            let str1 = value_to_string(value1, table, wrap1);
            let str2 = value_to_string(value2, table, wrap2);
            format!("{str1}{op}{str2}")
        }
        Value::Factorial(key1) | Value::DoubleFactorial(key1) | Value::Sqrt(key1) => {
            let value1 = *table.get(&key1).expect("Present");
            let str1 = value_to_string(value1, table, true);
            match value {
                Value::Factorial(_) => format!("{str1}!"),
                Value::DoubleFactorial(_) => format!("{str1}!!"),
                Value::Sqrt(_) => format!("s{str1}"),
                _ => unreachable!(),
            }
        }
        Value::TimesShift(key1, key2, shift) | Value::DivShift(key1, key2, shift) => {
            let value1 = *table.get(&key1).expect("Present");
            let value2 = *table.get(&key2).expect("Present");
            let str1 = value_to_string(value1, table, true);
            let str2 = value_to_string(value2, table, true);
            let shift = shift as usize;
            let op = match value {
                Value::TimesShift(_, _, _) => '*',
                Value::DivShift(_, _, _) => '/',
                _ => unreachable!(),
            };
            if str2.len() > shift {
                format!(
                    "{str1}{op}{}.{}",
                    &str2[..str2.len() - shift],
                    &str2[str2.len() - shift..]
                )
            } else {
                format!(
                    "{str1}{op}.{}{str2}",
                    (0..shift - str2.len()).map(|_| '0').collect::<String>()
                )
            }
        }
        _ => unimplemented!("{:?}", value),
    };
    if wrap && !matches!(value, Value::Direct(_)) {
        format!("({unwrapped_string})")
    } else {
        unwrapped_string
    }
}
fn main() {
    search()
}